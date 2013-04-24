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
            mNetServerTextBox.Text = Settings.NetServer;
            mNetPortTextBox.Text = Settings.NetPort;
            mNetUserTextBox.Text = Settings.NetUser;
            mNetDomainTextBox.Text = Settings.NetDomain;
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

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                SaveSettings();
                CheckSettings();

                UI.ShowDialog(this, new ReadyToInstallDialog());
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
                SaveSettings();

                UI.ShowDialog(this, new SqlDialog());
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }


        private void SaveSettings()
        {
            Settings.NetServer = this.mNetServerTextBox.Text;
            Settings.NetPort = this.mNetPortTextBox.Text;
            Settings.NetUser = this.mNetUserTextBox.Text;
            Settings.NetDomain = this.mNetDomainTextBox.Text;
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
