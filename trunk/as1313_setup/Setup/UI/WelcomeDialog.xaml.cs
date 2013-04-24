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
    /// Interaction logic for Welcome.xaml
    /// </summary>
    public partial class WelcomeDialog : Window
    {
        public WelcomeDialog()
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

        private void NextButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                if (Uninstall.IsAppInstalled())
                {
                    UI.ShowDialog(this, new UninstallDialog());
                }
                else
                {
                    UI.ShowDialog(this, new DestinationFolderDialog());
                }
            }
            catch (Exception ex)
            {
                Message.Show(ex);
            }
        }
    }
}
