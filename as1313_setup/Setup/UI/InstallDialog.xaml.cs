using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

using Setup.Common;

namespace Setup.UI
{
    /// <summary>
    /// Interaction logic for InstallDialog.xaml
    /// </summary>
    public partial class InstallDialog : Window
    {

        public InstallDialog()
        {
            InitializeComponent();

            // background thread
            this.backgroundWorker = (BackgroundWorker)this.FindResource("backgroundWoker");

            this.backgroundWorker.RunWorkerAsync();
            Mouse.OverrideCursor = Cursors.Wait;
        }

        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                if (Message.Question("Are you sure to abort installation?"))
                {
                    this.backgroundWorker.CancelAsync();
                    this.CancelButton.IsEnabled = false;
                }
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }


        /// <summary>
        ///  All operations completed successfully
        /// </summary>
        private void OnSuccess()
        {
            Mouse.OverrideCursor = null;
            General.ShowDialog(this, new FinishDialog());
        }

        /// <summary>
        ///  All operations completed with an error     
        /// </summary>
        private void OnError()
        {
            Message.Show("Rolling back");
            Install.Rollback();
            Mouse.OverrideCursor = null;
            General.ShowDialog(this, new FinishDialog());
        }
    }
}
