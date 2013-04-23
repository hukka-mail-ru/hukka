using System;
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
    /// Interaction logic for LibsFolderDialog.xaml
    /// </summary>
    public partial class LibsFolderDialog : Window
    {
        public LibsFolderDialog()
        {
            InitializeComponent();
            mFolderLabel.Content = Common.Settings.LibsFolder;
        }


        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            try
            { 
                General.CloseDialog(this);
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            try
            { 
                General.ShowDialog(this, new SqlDialog());
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }


        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            try
            { 
                General.ShowDialog(this, new DestinationFolderDialog());
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }


        private void BrowseButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                System.Windows.Forms.FolderBrowserDialog dialog = new System.Windows.Forms.FolderBrowserDialog();
                System.Windows.Forms.DialogResult result = dialog.ShowDialog();

                if (result == System.Windows.Forms.DialogResult.OK)
                {
                    mFolderLabel.Content = dialog.SelectedPath;
                    Settings.LibsFolder = dialog.SelectedPath;
                }
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }
    }
}
