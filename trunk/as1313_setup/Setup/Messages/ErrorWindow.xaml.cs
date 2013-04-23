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

namespace Setup.Messages
{
    /// <summary>
    /// Interaction logic for ErrorWindow.xaml
    /// </summary>
    public partial class ErrorWindow : Window
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ErrorWindow" /> class.
        /// </summary>
        public ErrorWindow()
        {
            this.InitializeComponent();
        }

        /// <summary>
        /// "Key pressed" event 
        /// </summary>
        /// <param name="sender">object sender</param>
        /// <param name="e">Event Args</param>
        private void OnKeyDownHandler(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Return || e.Key == Key.Escape)
            {
                this.Close();
            }
        }

        private void OKButton_Click(object sender, RoutedEventArgs e)
        {
            this.Close();
        }
    }
}
