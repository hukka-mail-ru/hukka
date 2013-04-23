// -----------------------------------------------------------------------
// <copyright file="MessageWindow.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Messages
{

    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Windows.Forms;

    /// <summary>
    /// A wrapper for showing of message 
    /// </summary>
    public class MessageWindow
    {
        /// <summary>
        /// Shows a window with error and backtrace
        /// </summary>
        /// <param name="generalInfo">the error description</param>
        /// <param name="detailInfo">the backtrace</param>
        public static void ShowErrorWindow(string generalInfo, string detailInfo)
        {
            ErrorWindow window = new ErrorWindow();

            window.tbGeneralInfo.Text = generalInfo;
            window.tbDetailInfo.Text = detailInfo;

            window.ShowInTaskbar = true;
            window.ShowDialog();
            window.Activate();
        }

        /// <summary>
        /// Shows a simple message
        /// </summary>
        /// <param name="message">the message</param>
        public static void ShowMessageBox(string message)
        {
            MessageBox.Show(message, "MESSAGE");
        }

        /// <summary>
        /// Shows a question
        /// </summary>
        /// <param name="message">the message</param>
        /// <returns>user's answer</returns>
        public static bool ShowQuestionBox(string message)
        {
            return MessageBox.Show(message, "QUESTION", MessageBoxButtons.YesNo) == DialogResult.Yes;
        }
    }
}
