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
            General.CloseDialog(this);
        }

        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            General.ShowDialog(this, new ServerDialog());
        }

        private void InstallButton_Click(object sender, RoutedEventArgs e)
        {

            this.Close();
        }
    }
}
