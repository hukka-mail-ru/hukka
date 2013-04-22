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
            if (General.SureToExit())
            {
                this.Close();
            }
        }

        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            ServerDialog dialog = new ServerDialog();
            dialog.Show();

            this.Close();
        }

        private void InstallButton_Click(object sender, RoutedEventArgs e)
        {

            this.Close();
        }
    }
}
