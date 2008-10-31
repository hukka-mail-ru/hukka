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
 
MODULE_LICENSE("Dual BSD/GPL");

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
    printk(KERN_WARNING "hello: syscall 'write'...\n");
    
    down(&sem);    
    
    // f_pos is ignored. The data is always written to the beginning of the 'memory'   
    kfree(memory);
    memory = kmalloc(count, GFP_KERNEL);
    if(!memory)
    {
        res = -ENOMEM;
        goto nax;
    }
    
    memset(memory, 0, count);
    
    if (copy_from_user (memory, buf, count)) 
    {
        res = -EFAULT;
        goto nax;
    }
    
    mem_size = count;
    
    printk(KERN_WARNING "hello: wrote bytes: %d\n", count);
    
    res = count; // return as much as asked :)
    
    
nax:
    up(&sem);
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


static int startup(void)
{
    dev_t dev = MKDEV(major, minor);
    int result = 0;
    

    printk(KERN_INFO "hello: startup\n");
  //  printk(KERN_INFO "The process is '%s' (pid %i)\n", current->comm, current->pid);
 //   printk(KERN_INFO "The kernel is %i\n", LINUX_VERSION_CODE);
    
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


    printk(KERN_WARNING "hello: got version %d:%d\n", major, minor);
    printk(KERN_WARNING "hello: my_cdev allocated\n");
    printk(KERN_WARNING "hello: my_cdev added\n");

    return 0;
}

static void kickoff(void)
{
    dev_t dev = MKDEV(major, minor);
    
    kfree(memory);

    cdev_del(my_cdev);

    unregister_chrdev_region(dev, 1); // we had only 1 device

    printk(KERN_INFO "hello: kicked off\n");
}




module_init(startup);
module_exit(kickoff);
