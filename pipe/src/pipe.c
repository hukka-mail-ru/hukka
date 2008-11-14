/*  
 *  pipe-1.c - The simplest kernel module.
 */

#include <linux/init.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/version.h>
#include <linux/kdev_t.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/uaccess.h>
#include <linux/ioport.h>
#include <asm/system.h>
#include <asm/io.h>
#include <linux/proc_fs.h> // proc
#include <linux/ioctl.h> /* needed for the _IOW etc stuff used later */

 
MODULE_LICENSE("Dual BSD/GPL");


// port parameters
/* use 8 ports by default */
#define SHORT_NR_PORTS  8  
//static unsigned long lpt_port = 0x378;


// program parameters
static int major = 0;
static int minor = 0;
module_param(major, int, S_IRUGO);
module_param(minor, int, S_IRUGO);


// structures 

static DECLARE_MUTEX(sem);
static DECLARE_WAIT_QUEUE_HEAD(inq);
static DECLARE_WAIT_QUEUE_HEAD(outq);



static struct cdev* my_cdev;

// memory buffer
#define BUF_SIZE 1024
char buf[BUF_SIZE];


char* buffer = buf;   
char* end = buf + BUF_SIZE;                 /* begin of buf, end of buf */

int buffersize = BUF_SIZE;           /* used in pointer arithmetic */

char* rp = buf;
char* wp = buf;                     /* where to read, where to write */




// functions
int pipe_open(struct inode *inode, struct file *filp)
{
    return 0;          /* success */
}

int pipe_release(struct inode *inode, struct file *filp)
{
    return 0;
}

//////////////// READ ///////////////////////////////////////////

ssize_t pipe_read(struct file *filp, char __user *buf, size_t count,
           loff_t *f_pos)
{
    if (down_interruptible(&sem))
        return -ERESTARTSYS;

    // BLOCK
    while (rp == wp) // nothing to read
    { 
        up(&sem); // release the lock 
        
        if (filp->f_flags & O_NONBLOCK)
            return -EAGAIN;
        
        printk("\"%s\" reading: going to sleep\n", current->comm);
        
        if (wait_event_interruptible(inq, (rp != wp)))
            return -ERESTARTSYS; // signal: tell the fs layer to handle it 
        
        /* otherwise loop, but first reacquire the lock */
        if (down_interruptible(&sem))
            return -ERESTARTSYS;
    }
    
    
    // RETURN DATA
    if (wp > rp)
        count = min(count, (size_t)(wp - rp));
    else /* the write pointer has wrapped, return data up to dev->end */
        count = min(count, (size_t)(end - rp));
    
    if (copy_to_user(buf, rp, count)) 
    {
        up (&sem);
        return -EFAULT;
    }
    
    rp += count; // increment and wrap
    if (rp == end)
        rp = buffer; 
    
    up (&sem);

    
    // AWAKE WRITERS
    wake_up_interruptible(&outq);
    printk("\"%s\" did read %li bytes\n",current->comm, (long)count);
    return count;
}


static int spacefree(void)
{
    if (rp == wp)
        return buffersize - 1;
    return ((rp + buffersize - wp) % buffersize) - 1;
}

//////////////// WRITE ///////////////////////////////////////////

ssize_t pipe_write(struct file *filp, const char __user *buf, size_t count, loff_t *f_pos) 
{
    int result = 0;
    if (down_interruptible(&sem))
        return -ERESTARTSYS;

    // Make sure there's space to write 
    while (spacefree() == 0) // full  
    { 
        
        up(&sem);
        if (filp->f_flags & O_NONBLOCK)
            return -EAGAIN;
        
        if (wait_event_interruptible(inq, (rp != wp)))
            return -ERESTARTSYS; // signal: tell the fs layer to handle it 
        
        if (down_interruptible(&sem))
            return -ERESTARTSYS;
    }
 
    if (result)
        return result; // scull_getwritespace called up(&dev->sem) 

    // ok, space is there, accept something 
    count = min(count, (size_t)spacefree());
    
    if (wp >= rp)
        count = min(count, (size_t)(end - wp)); // to end-of-buf 
    else // the write pointer has wrapped, fill up to rp-1 
        count = min(count, (size_t)(rp - wp - 1));
    
    printk("Going to accept %li bytes to %p from %p\n", (long)count, wp, buf);
    if (copy_from_user(wp, buf, count)) 
    {
        up (&sem);
        return -EFAULT;
    }
    
    wp += count;
    if (wp == end)
        wp = buffer; // wrapped 
    
    up(&sem);

    printk(KERN_DEBUG "process %i (%s) awakening the readers...\n",
	   current->pid, current->comm);

    wake_up_interruptible(&inq);
    return count; /* succeed, to avoid retrial */
    
}

//////////////// IOCTL ///////////////////////////////////////////
int pipe_ioctl(struct inode *inode, struct file *filp,
		unsigned int cmd, unsigned long arg)
{
    return 0;
}



//////////////// FILE OPERATIONS ///////////////////////////////////////////

static struct file_operations my_ops = 
{
    .owner      = THIS_MODULE,
    .read       = pipe_read,
    .write      = pipe_write,
    .open       = pipe_open,
    .release    = pipe_release,
    .ioctl      = pipe_ioctl    
};


//////////////// PROC ///////////////////////////////////////////

int pipe_read_procmem(char *buf, char **start, off_t offset,
		       int count, int *eof, void *data)
{
    int len = 0;

    down(&sem);

    /*
    len += sprintf(buf+len, "The process is '%s' (pid %i)\n",
		   current->comm, current->pid);
    if(memory)
    {
        len += sprintf(buf+len, "Allocated %d bytes\n", mem_size);
    }
    else
    {
        len += sprintf(buf+len, "No memory in use\n");
    }*/
    
    up(&sem);

    *eof = 1;
    return len;
}

static void pipe_create_proc(void)
{
    create_proc_read_entry("pipemem", 0 /* default mode */,
			   NULL /* parent dir */, pipe_read_procmem,
      NULL /* client data */);
}

static void pipe_remove_proc(void)
{
    remove_proc_entry("pipemem", NULL /* parent dir */);
}
//////////////// EO PROC ///////////////////////////////////////////





static int startup(void)
{
    dev_t dev = MKDEV(major, minor);
    int result = 0;

    printk(KERN_INFO "pipe: startup\n");


    // get a driver number
    if (major) 
    {
	dev = MKDEV(major, minor);
	result = register_chrdev_region(dev, 1, "pipe");
    } 
    else 
    {
	result = alloc_chrdev_region(&dev, minor, 1, "pipe");
	major = MAJOR(dev);
    }
    
    if (result < 0) 
    {
	printk(KERN_WARNING "pipe: can't get version %d:%d\n", major, minor);
	return result;
    }

       
    // Initialize the device.	
    my_cdev = cdev_alloc();
    if(!my_cdev)
    {
	printk(KERN_WARNING "pipe:  cdev_alloc failed");
	return -1;
    }

    my_cdev->ops = &my_ops;
    
    if(cdev_add(my_cdev, dev, 1) < 0)
    {
	printk(KERN_WARNING "pipe:  cdev_add failed");
	return -1;
    }

    
    pipe_create_proc(); // proc debugging

    printk(KERN_WARNING "pipe: got version %d:%d\n", major, minor);
    printk(KERN_WARNING "pipe: my_cdev allocated\n");
    printk(KERN_WARNING "pipe: my_cdev added\n");
    
    

    

    

    return 0;
}

static void kickoff(void)
{
    dev_t dev = MKDEV(major, minor);
    
    pipe_remove_proc(); // proc debugging
    
    cdev_del(my_cdev); // Denitialize the device

    unregister_chrdev_region(dev, 1); // we had only 1 device

    printk(KERN_INFO "pipe: kicked off\n");
}




module_init(startup);
module_exit(kickoff);
