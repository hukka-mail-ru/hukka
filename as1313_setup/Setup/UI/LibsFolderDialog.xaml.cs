using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Forms;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

using Setup.Common;

namespace Setup.UI
{
    /// <summary>
    /// Interaction logic for LibsFolderDialog.xaml
    /// </summary>
    public partial class LibsFolderDialog : Window
    {
        public LibsFolderDialog()
        {
            InitializeComponent();
            FolderLabel.Content = Common.Settings.LibsFolder;
        }


        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            if (General.SureToExit())
            {
                this.Close();
            }
        }

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            SqlDialog dialog = new SqlDialog();
            dialog.Show();

            this.Close();
        }


        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            DestinationFolderDialog dialog = new DestinationFolderDialog();
            dialog.Show();

            this.Close();
        }


        private void BrowseButton_Click(object sender, RoutedEventArgs e)
        {
            FolderBrowserDialog dialog = new FolderBrowserDialog();
            System.Windows.Forms.DialogResult result = dialog.ShowDialog();

            if (result == System.Windows.Forms.DialogResult.OK)
            {
                FolderLabel.Content = dialog.SelectedPath;
                Settings.LibsFolder = dialog.SelectedPath;
            }
        }
    }
}
