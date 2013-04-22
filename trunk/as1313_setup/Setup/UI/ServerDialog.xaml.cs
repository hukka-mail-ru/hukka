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
    /// Interaction logic for ServerDialog.xaml
    /// </summary>
    public partial class ServerDialog : Window
    {
        public ServerDialog()
        {
            InitializeComponent();
            NetServerTextBox.Text = Settings.NetServer;
            NetPortTextBox.Text = Settings.NetPort;
            NetUserTextBox.Text = Settings.NetUser;
            NetDomainTextBox.Text = Settings.NetDomain;
        }

        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            General.CloseDialog(this);
        }

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                SaveSettings();
                CheckSettings();

                General.ShowDialog(this, new ReadyToInstallDialog());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            SaveSettings();

            General.ShowDialog(this, new SqlDialog());
        }

        private void SaveSettings()
        {
            Settings.NetServer = this.NetServerTextBox.Text;
            Settings.NetPort = this.NetPortTextBox.Text;
            Settings.NetUser = this.NetUserTextBox.Text;
            Settings.NetDomain = this.NetDomainTextBox.Text;
        }

        void CheckSettings()
        {
            if (Settings.NetServer == "")
            {
                throw new ExceptionNoUserInput("server address");
            }

            if (Settings.NetPort == "")
            {
                throw new ExceptionNoUserInput("server port");
            }

            if (Settings.NetUser == "")
            {
                throw new ExceptionNoUserInput("user name");
            }

            if (Settings.NetDomain == "")
            {
                throw new ExceptionNoUserInput("domain name");
            }
        }
    }
}
