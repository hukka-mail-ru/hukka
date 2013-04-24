// -----------------------------------------------------------------------
// <copyright file="MyException.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;

    /// <summary>
    /// The base class for all the exceptions of our application
    /// </summary>
    public class MyException : System.Exception
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="MyException" /> class.
        /// </summary>
        /// <param name="message">error message</param>
        public MyException(string message) :
            base(message)
        {
        }
    }


    public class ExceptionNoUserInput : MyException
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ExceptionItemNotExpandable" /> class.
        /// </summary>
        /// <param name="item">the item</param>
        public ExceptionNoUserInput(string what) :
            base("Please input " + what + ".")
        {
        }
    }


    public class ExceptionSqlError : MyException
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ExceptionItemNotExpandable" /> class.
        /// </summary>
        /// <param name="item">the item</param>
        public ExceptionSqlError(string what) :
            base("SQL Error. Please check your DB credentials and permissions.\n\n" + what)
        {
        }
    }

    public class ExceptionInUninstaller : MyException
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ExceptionItemNotExpandable" /> class.
        /// </summary>
        /// <param name="item">the item</param>
        public ExceptionInUninstaller(string what) :
            base("Error creating uninstaller.\n\n" + what)
        {
        }
    }
}
