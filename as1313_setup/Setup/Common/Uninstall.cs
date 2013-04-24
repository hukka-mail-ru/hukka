// -----------------------------------------------------------------------
// <copyright file="Uninstall.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using Microsoft.Win32;
    using System.Text;
    using System.Reflection;


    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class Uninstall
    {

        public static void CreateUninstaller()
        {
            using (RegistryKey parent = Registry.LocalMachine.OpenSubKey(
                         @"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall", true))
            {
                if (parent == null)
                {
                    throw new ExceptionInUninstaller("Uninstall registry key not found.");
                }
                try
                {
                    RegistryKey key = null;
                    try
                    {
                        key = parent.OpenSubKey(Settings.ProductName, true) ??
                              parent.CreateSubKey(Settings.ProductName);

                        if (key == null)
                        {
                            throw new ExceptionInUninstaller("Unable to create uninstaller");
                        }

                        Message.Show("Creating uninstaller " + key.ToString());

                     //   Assembly asm = GetType().Assembly;
                        string version = General.GetAppVersion();
                        string exe = Path.Combine(Settings.VersionDir, Settings.MainExecutable);
                     //   string exe = "\"" + asm.CodeBase.Substring(8).Replace("/", "\\\\") + "\"";

                        key.SetValue("DisplayName", Settings.ProductName);
                        key.SetValue("ApplicationVersion", version);
                        key.SetValue("Publisher", Settings.CompanyName);
                      //  key.SetValue("DisplayIcon", exe);
                        key.SetValue("DisplayVersion", version);
                        // key.SetValue("URLInfoAbout", "http://www.blinemedical.com");
                        // key.SetValue("Contact", "support@mycompany.com");
                        key.SetValue("InstallDate", DateTime.Now.ToString("yyyyMMdd"));
                        key.SetValue("UninstallString", exe + " /uninstallprompt");
                    }
                    finally
                    {
                        if (key != null)
                        {
                            key.Close();
                        }
                    }
                }
                catch (Exception ex)
                {
                    throw new ExceptionInUninstaller(ex.Message);
                }
            }
        }
    }
}
