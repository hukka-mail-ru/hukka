// -----------------------------------------------------------------------
// <copyright file="Message.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Linq;
    using System.Text;

    using Setup.Messages;
  /// <summary>
    /// Log message
    /// </summary>
    public class Message
    {
        /// <summary>
        /// Cleares the console
        /// </summary>
        public static void ClearScreen()
        {
            Console.Clear();
        }

        /// <summary>
        /// Output an exception into console
        /// </summary>
        /// <param name="e">Exception object</param>
        public static void Log(System.Exception e)
        {
            string header = (e.GetType() == typeof(MyException) ||
                    e.GetType().BaseType == typeof(MyException)) ? "ERROR" : "UNEXPECTED ERROR";

            Console.ForegroundColor = ConsoleColor.Red;
            string str = "---------\n" +
                         header + "! " + e.Message + "\n" +
                         "(EXCEPTION: " + e.GetType() + ")\n";

            Console.WriteLine(str);
            Trace.WriteLine(str);

            // Backtrace
            Console.ForegroundColor = ConsoleColor.DarkRed;

            Console.WriteLine(e.StackTrace);
            Trace.WriteLine(e.StackTrace);

            Console.ResetColor();
        }

        /// <summary>
        /// Outputs a highlighted log message
        /// </summary>
        /// <param name="message">message to output</param>
        public static void LogHi(string message)
        {
            Log(message, ConsoleColor.DarkGreen, 2);
        }

        /// <summary>
        /// Output the backtrace followed by a message into console
        /// </summary>
        /// <param name="message">message to output</param>
        /// <param name="color">color of the message</param>
        /// <param name="skipFrames">number of frames in the backtrace to omit</param>
        public static void Log(string message, ConsoleColor color = ConsoleColor.Black, int skipFrames = 1)
        {
            if (color != ConsoleColor.Black)
            {
                Console.ForegroundColor = color;
            }

            StackFrame callStack = new StackFrame(skipFrames, true);
            message = string.Format(
                            "{0:HH:mm:ss} [{2,4}] {1,-18}\t{3,-30}\t{4}",
                            DateTime.Now,
                            Path.GetFileName(callStack.GetFileName()),
                            callStack.GetFileLineNumber(),
                            callStack.GetMethod().Name,
                            message);

            Console.WriteLine(message);
            Debug.WriteLine(message);

            Console.ResetColor();
        }

        /// <summary>
        /// Output an exception onto screen (and write a log)
        /// </summary>
        /// <param name="e">Exception object</param>
        public static void Show(System.Exception e)
        {
            Log(e);

            string generalInfo = string.Empty;
            string detailInfo = string.Empty;

            if (e.GetType() == typeof(MyException) ||
                e.GetType().BaseType == typeof(MyException))
            {
                generalInfo = e.Message;
                detailInfo = e.StackTrace;
            }
            else
            {
                generalInfo = "Unexpected error occurs.\n\n" +
                    "Try this action again.\n" +
                    "If the problem persists, please contact the support.";
                detailInfo = "Error: " + e.Message + "\n\n" + e.StackTrace;
            }

            MessageWindow.ShowErrorWindow(generalInfo, detailInfo);

        }

        /// <summary>
        /// Output a message onto screen (and write a log)
        /// </summary>
        /// <param name="message">a message to output</param>
        public static void Show(string message)
        {
            try
            {
                Log("GUI MESSAGE: " + message, ConsoleColor.DarkBlue, 2);

                // We should use .Net MessageBox instead of NXMessageBox
                // because NXMessageBox minimizes all other open forms
                MessageWindow.ShowMessageBox(message);
            }
            catch (Exception e)
            {
                Log(e);
            }
        }

        /// <summary>
        /// Output a question box onto screen (and write a log)
        /// </summary>
        /// <param name="message">a question to output</param>
        /// <returns>true if user presses "YES", false otherwise</returns>

        public static bool Question(string message)
        {
            try
            {
                Log("GUI QUESTION: " + message, ConsoleColor.DarkBlue, 2);
                return MessageWindow.ShowQuestionBox(message);
            }
            catch (Exception e)
            {
                Log(e);
                return false;
            }
        }

    }
   
}
