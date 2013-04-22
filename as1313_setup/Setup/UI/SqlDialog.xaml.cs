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
    /// Interaction logic for SqlDialog.xaml
    /// </summary>
    public partial class SqlDialog : Window
    {
        public SqlDialog()
        {
            InitializeComponent();
            SQLServerTextBox.Text = Settings.SQLServer;
            SQLUserTextBox.Text = Settings.SQLUser;
            SQLPasswordTextBox.Password = Settings.SQLPassword;
        }

        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            General.CloseDialog(this);
        }

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            SaveSettings();

            if (!CheckSettings())
            {
                return;
            }

            General.ShowDialog(this, new ServerDialog());
        }

        private void BackButton_Click(object sender, RoutedEventArgs e)
        {
            SaveSettings();

            General.ShowDialog(this, new LibsFolderDialog());
        }



        private void SaveSettings()
        {
            Settings.SQLServer = this.SQLServerTextBox.Text;
            Settings.SQLUser = this.SQLUserTextBox.Text;
            Settings.SQLPassword = this.SQLPasswordTextBox.Password;
        }

        bool CheckSettings()
        {
            if (Settings.SQLServer == "")
            {
                MessageBox.Show("Please input SQL server instance name.");
                return false;
            }

            if (Settings.SQLUser == "")
            {
                MessageBox.Show("Please input SQL user name.");
                return false;
            }

            return true;
        }
    }
}
