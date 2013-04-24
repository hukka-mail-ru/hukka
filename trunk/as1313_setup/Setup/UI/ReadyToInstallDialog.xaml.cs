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
    /// Interaction logic for ReadyToInstall.xaml
    /// </summary>
    public partial class ReadyToInstallDialog : Window
    {
        public ReadyToInstallDialog()
        {
            InitializeComponent();
        }


        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                UI.CloseDialog(this);
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
                UI.ShowDialog(this, new ServerDialog());
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }


        private void InstallButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                UI.ShowDialog(this, new InstallDialog());
            }
            catch (Exception ex)
            {
                Message.Show(ex);
                Mouse.OverrideCursor = null;
            }
        }
    }
}
