﻿using System;
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
            this.Close();
        }

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            SaveSettings();

            ReadyToInstallDialog dialog = new ReadyToInstallDialog();
            dialog.Show();

            this.Close();

        }

        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            SaveSettings();

            SqlDialog dialog = new SqlDialog();
            dialog.Show();

            this.Close();
        }

        private void SaveSettings()
        {
            Settings.NetServer = this.NetServerTextBox.Text;
            Settings.NetPort = this.NetPortTextBox.Text;
            Settings.NetUser = this.NetUserTextBox.Text;
            Settings.NetDomain = this.NetDomainTextBox.Text;
        }
    }
}
