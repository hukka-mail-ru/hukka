// -----------------------------------------------------------------------
// <copyright file="General.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.UI
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;

    using Setup.Common;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class UI
    {
        public static void ShowDialog(System.Windows.Window thisDialog, System.Windows.Window nextDialog)
        {
            nextDialog.Show();
            thisDialog.Close();
        }


        public static void CloseDialog(System.Windows.Window dialog)
        {
            if (Message.Question("Are you sure you want to exit the Install Wizard?"))
            {
                dialog.Close();
            }
        }
    }
}
