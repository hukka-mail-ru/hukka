// -----------------------------------------------------------------------
// <copyright file="Files.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Text;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class Files
    {
        public static void Copy()
        {
            CreateFolders();
            CopyDirectory(Settings.SourceDir, Settings.VersionDir);
        }


        public static void Delete()
        {
            DeleteFilesAndDirectory(Settings.VersionDir);
        }


        private static void DeleteFilesAndDirectory(string target_dir)
        {
            string[] files = Directory.GetFiles(target_dir);
            string[] dirs = Directory.GetDirectories(target_dir);

            foreach (string file in files)
            {
                File.SetAttributes(file, FileAttributes.Normal);
                File.Delete(file);
            }

            foreach (string dir in dirs)
            {
                DeleteFilesAndDirectory(dir);
            }

            Directory.Delete(target_dir, false);
        }


        private static void CreateFolders()
        {
            // company dir
            string companyDir = Path.Combine(Settings.DestinationFolder, Settings.CompanyName);
            if (!Directory.Exists(companyDir))
            {
                Directory.CreateDirectory(companyDir);
            }

            // product dir
            string productDir = Path.Combine(companyDir, Settings.SolutionName);
            if (!Directory.Exists(productDir))
            {
                Directory.CreateDirectory(productDir);
            }

            // version dir
            Settings.VersionDir = Path.Combine(productDir, General.GetAppVersion());

            if (!Directory.Exists(Settings.VersionDir))
            {
                Directory.CreateDirectory(Settings.VersionDir);
            }
        }


        private class Folders
        {
            public string Source { get; private set; }
            public string Target { get; private set; }

            public Folders(string source, string target)
            {
                Source = source;
                Target = target;
            }
        }

        private static void CopyDirectory(string source, string target)
        {
            var stack = new Stack<Folders>();
            stack.Push(new Folders(source, target));

            while (stack.Count > 0)
            {
                var folders = stack.Pop();
                Directory.CreateDirectory(folders.Target);
                foreach (var file in Directory.GetFiles(folders.Source, "*.*"))
                {
                    string targetFile = Path.Combine(folders.Target, Path.GetFileName(file));
                    if (File.Exists(targetFile)) File.Delete(targetFile);
                    File.Copy(file, targetFile);
                }

                foreach (var folder in Directory.GetDirectories(folders.Source))
                {
                    stack.Push(new Folders(folder, Path.Combine(folders.Target, Path.GetFileName(folder))));
                }
            }
        }
    }
}
