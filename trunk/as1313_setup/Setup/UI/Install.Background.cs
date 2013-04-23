// -----------------------------------------------------------------------
// <copyright file="ReadyToInstall.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.UI
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel;
    using System.Linq;
    using System.Text;
    using System.Threading;
    using System.Windows.Input;

    using Setup.Common;
    /// <summary>
    /// LoginWindow Background thread
    /// </summary>
    public partial class InstallDialog
    {
        /// <summary>
        /// The thread for BackEnd operatios
        /// </summary>
        private BackgroundWorker backgroundWorker;

        /// <summary>
        /// A handler to backgroundWorker.RunWorkerAsync
        /// </summary>
        /// <param name="sender">not used</param>
        /// <param name="e">the arguments </param>
        private void BackgroundWorker_DoWork(object sender, System.ComponentModel.DoWorkEventArgs e)
        {
            BackgroundWorker worker = sender as BackgroundWorker;
            for (int i = 0; i < 100; i++)
            {
                Thread.Sleep(100);
            }
            // exceptions are catched by BackgroundWorker_RunWorkerCompleted
            Install.Go();
        }

        /// <summary>
        /// A hander to the end of the BackgroundWorker work
        /// </summary>
        /// <param name="sender">not used</param>
        /// <param name="e">a status</param>
        private void BackgroundWorker_RunWorkerCompleted(object sender, System.ComponentModel.RunWorkerCompletedEventArgs e)
        {
            Mouse.OverrideCursor = null;

            if (e.Cancelled == true)
            {
                Message.Show("Operation cancelled by user");
                this.OnError();
            }
            else if (!(e.Error == null))
            {
                Message.Show(e.Error);
                this.OnError();
            }
            else
            {
                this.OnSuccess();
            }
        }
    }
}
