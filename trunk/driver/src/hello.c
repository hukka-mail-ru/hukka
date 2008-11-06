/*  
 *  hello-1.c - The simplest kernel module.
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
 
MODULE_LICENSE("Dual BSD/GPL");

#define SHORT_NR_PORTS  8   /* use 8 ports by default */
static unsigned long lpt_port = 0x378;

// program parameters
static int major = 0;
static int minor = 0;

module_param(major, int, S_IRUGO);
module_param(minor, int, S_IRUGO);

// structures 
//struct semaphore sem;
static DECLARE_MUTEX(sem);

static struct cdev* my_cdev;

// memory buffer
static char* memory = NULL;
static size_t mem_size = 0;


// functions

int hello_open(struct inode *inode, struct file *filp)
{
    return 0;          /* success */
}

int hello_release(struct inode *inode, struct file *filp)
{
    return 0;
}





//////////////// WRITE ///////////////////////////////////////////

ssize_t hello_write(struct file *filp, const char __user *buf, size_t count,
                    loff_t *f_pos) 
{
    
    int res = -ENOMEM;
    printk(KERN_WARNING "hello: hello_write: started...\n");
    
    down(&sem);    
    
    // f_pos is ignored. The data is always written to the beginning of the 'memory'   
    kfree(memory);
    memory = kmalloc(count, GFP_KERNEL);
    if(!memory)
    {
        res = -ENOMEM;
        printk(KERN_WARNING "hello: hello_write: kmalloc() failed \n");
        goto nax;
    }
    
    memset(memory, 0, count);
    
    if (copy_from_user (memory, buf, count)) 
    {
        res = -EFAULT;
        printk(KERN_WARNING "hello: hello_write: copy_from_user() failed \n");
        goto nax;
    }
    
    mem_size = count;
    res = count; // return as much as asked :)
    
    printk(KERN_WARNING "hello: wrote bytes: %d\n", count);
    
/*
    // write to port
    while (count--) 
    {
        outb(*(memory++), lpt_port);
        wmb();
    }
   */ 
nax:

    up(&sem);
    printk(KERN_WARNING "hello: hello_write finished. Return value: %d\n", res);
    return res;
}

//////////////// READ ///////////////////////////////////////////

ssize_t hello_read(struct file *filp, char __user *buf, size_t count,
                   loff_t *f_pos)
{
    int res = -ENOMEM;
    down(&sem); 
    
    // EOF case
    if (*f_pos > mem_size)
    {
        res = 0;
        goto nax;
    }
    
    // UP-TO-EOF case
    if (*f_pos + count > mem_size)
    {
        count = mem_size - *f_pos; 
    }

    if(copy_to_user(buf, memory + *f_pos, count))
    {
        res = -EFAULT;
        goto nax;
    }
    
    *f_pos += count;
 
    printk(KERN_WARNING "hello: Read KU-KU! count=%d\n", count);
    
    res = count; // maybe we didn't read everything    
    
nax:
    up(&sem);
    return res;
}


static struct file_operations my_ops = 
{
    .owner      = THIS_MODULE,
    .read       = hello_read,
    .write      = hello_write,
    .open       = hello_open,
    .release    = hello_release
};


//////////////// PROC ///////////////////////////////////////////

int hello_read_procmem(char *buf, char **start, off_t offset,
                       int count, int *eof, void *data)
{
    int len = 0;

    down(&sem);

    len += sprintf(buf+len, "The process is '%s' (pid %i)\n",
                             current->comm, current->pid);
    if(memory)
    {
        len += sprintf(buf+len, "Allocated %d bytes\n", mem_size);
    }
    else
    {
        len += sprintf(buf+len, "No memory in use\n");
    }
    
    up(&sem);

    *eof = 1;
    return len;
}

static void hello_create_proc(void)
{
    create_proc_read_entry("hellomem", 0 /* default mode */,
            NULL /* parent dir */, hello_read_procmem,
            NULL /* client data */);
}

static void hello_remove_proc(void)
{
    remove_proc_entry("hellomem", NULL /* parent dir */);
}
//////////////// EO PROC ///////////////////////////////////////////





static int startup(void)
{
    dev_t dev = MKDEV(major, minor);
    int result = 0;
    memory = NULL;
    

    printk(KERN_INFO "hello: startup\n");
    
    /*
    if (! request_region(lpt_port, SHORT_NR_PORTS, "LPT")) 
    {
        printk(KERN_INFO "hello: can't get I/O mem address 0x%lx\n", lpt_port);
        return -ENODEV;
    }
    printk(KERN_WARNING "hello: request_region: port 0x%lx hooked\n", lpt_port);
    */

    // get a driver number
    if (major) 
    {
        dev = MKDEV(major, minor);
        result = register_chrdev_region(dev, 1, "hello");
    } 
    else 
    {
        result = alloc_chrdev_region(&dev, minor, 1, "hello");
        major = MAJOR(dev);
    }
    
    if (result < 0) 
    {
        printk(KERN_WARNING "hello: can't get version %d:%d\n", major, minor);
        return result;
    }

       
    // Initialize the device.	
    my_cdev = cdev_alloc();
    if(!my_cdev)
    {
       printk(KERN_WARNING "hello:  cdev_alloc failed");
       return -1;
    }

    my_cdev->ops = &my_ops;
    
    if(cdev_add(my_cdev, dev, 1) < 0)
    {
       printk(KERN_WARNING "hello:  cdev_add failed");
       return -1;
    }

    
    hello_create_proc(); // proc debugging

    printk(KERN_WARNING "hello: got version %d:%d\n", major, minor);
    printk(KERN_WARNING "hello: my_cdev allocated\n");
    printk(KERN_WARNING "hello: my_cdev added\n");
    
    

    

    

    return 0;
}

static void kickoff(void)
{
    dev_t dev = MKDEV(major, minor);
    
    hello_remove_proc(); // proc debugging
    
    //release_region(lpt_port, SHORT_NR_PORTS);
    
    kfree(memory);
    
    memory = NULL;

    cdev_del(my_cdev); // Denitialize the device

    unregister_chrdev_region(dev, 1); // we had only 1 device

    printk(KERN_INFO "hello: kicked off\n");
}




module_init(startup);
module_exit(kickoff);
