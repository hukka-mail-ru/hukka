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
    /// Interaction logic for FinishDialog.xaml
    /// </summary>
    public partial class FinishDialog : Window
    {
        public FinishDialog()
        {
            InitializeComponent();
        }

        private void FinishButton_Click(object sender, RoutedEventArgs e)
        {
            this.Close();
        }
    }
}
