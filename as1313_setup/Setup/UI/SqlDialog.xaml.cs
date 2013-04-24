using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.IO;
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
            mSQLServerTextBox.Text = Settings.SQLServer;
            mSQLUserTextBox.Text = Settings.SQLUser;
            mSQLPasswordTextBox.Password = Settings.SQLPassword;
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

                CheckSqlServer();

                UI.ShowDialog(this, new ServerDialog());
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

                UI.ShowDialog(this, new LibsFolderDialog());
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }



        private void SaveSettings()
        {
            Settings.SQLServer = this.mSQLServerTextBox.Text;
            Settings.SQLUser = this.mSQLUserTextBox.Text;
            Settings.SQLPassword = this.mSQLPasswordTextBox.Password;
        }

        private void CheckSettings()
        {
            if (Settings.SQLServer == "")
            {
                throw new ExceptionNoUserInput("SQL server instance name");
            }

            if (Settings.SQLUser == "")
            {
                throw new ExceptionNoUserInput("SQL user name");
            }
        }


        private void CheckSqlServer()
        {
            string query = "IF EXISTS (SELECT name FROM sys.databases WHERE name = N'TEST_DB') DROP DATABASE TEST_DB; ";
            string fullQuery = query + " CREATE DATABASE TEST_DB; " + query;

            
            General.SqlQuery(fullQuery);
        }
    }
}
