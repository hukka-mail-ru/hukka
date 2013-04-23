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

        private Exception lastException; 

        /// <summary>
        /// A handler to backgroundWorker.RunWorkerAsync
        /// </summary>
        /// <param name="sender">not used</param>
        /// <param name="e">the arguments </param>
        private void BackgroundWorker_DoWork(object sender, 
            System.ComponentModel.DoWorkEventArgs e)
        {
            try
            {
                BackgroundWorker worker = sender as BackgroundWorker;
                // exceptions are catched by BackgroundWorker_RunWorkerCompleted

                Install.CreateFolders();
                if (backgroundWorker.CancellationPending)
                {
                    e.Cancel = true;
                    return;
                }

                Install.CopyFiles();
                if (backgroundWorker.CancellationPending)
                {
                    e.Cancel = true;
                    return;
                }

                Install.RunSqlScript();
                if (backgroundWorker.CancellationPending)
                {
                    e.Cancel = true;
                    return;
                }

                Install.ReplaceConfig();
            }
            catch (Exception ex)
            {
                e.Cancel = true;
                lastException = ex;
                return;
            }
        }

        /// <summary>
        /// A hander to the end of the BackgroundWorker work
        /// </summary>
        /// <param name="sender">not used</param>
        /// <param name="e">a status</param>
        private void BackgroundWorker_RunWorkerCompleted(object sender, System.ComponentModel.RunWorkerCompletedEventArgs e)
        {
            
            if (e.Cancelled == true)
            {           
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
