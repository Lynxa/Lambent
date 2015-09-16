using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CrystalFire
{
    class KVPGrinder
    {

        protected static KVP IsolateValuepair(String piece)
        {
            if (piece[0].Equals('(') && piece[piece.Length - 1].Equals(')') && piece.IndexOf(',') > 0)
            {
                String s1 = piece.Substring(1, piece.IndexOf(',') - 1);
                s1 = s1.Replace("\"", "");
                String s2 = piece.Substring(piece.IndexOf(',') + 1, piece.Length - piece.IndexOf(',') - 2);
                if (s2[0].Equals('[') && s2[s2.Length - 1].Equals(']'))
                {
                    return new KVP(s1, IsolateList(s2));
                }
                return new KVP(s1, s2);
            }
            else
                throw new Exception("Format!");

        }

        public static KVP DecipherLine(String msg)
        {
            try
            {
                String line = msg.Replace(" ", "");
                line = line.Replace("\r", "");
                line = line.Replace("\n", "");
                StringBuilder ss = new StringBuilder(line); //proverit', ne odli li tam chasy
                ss.Remove(0, 6); // length of "state(["
                ss.Remove(ss.Length - 2, 2); //length of ");"
                KVP root = new KVP("state", IsolateList(ss.ToString()));
                return root;
            }
            catch (Exception)
            {
                return null;
            }
        }

        private static List<KVP> IsolateList(string src)
        {
            List<KVP> lst = new List<KVP>();
            StringBuilder sd = new StringBuilder(src.Substring(1, src.Length - 2));
            StringBuilder tStr = new StringBuilder();
            int ind = 0;
            Stack<Char> st = new Stack<Char>();
            while (!sd.ToString().Equals(""))
            {

                while ((st.Count() != 0 && ind < sd.Length) || (ind == 0))
                {
                    if (st.Count > 0)
                    {
                        if (st.Peek() != '"')
                        {
                            if (sd[ind] == '(') st.Push('(');
                            if (sd[ind] == ')') st.Pop();
                            if (sd[ind] == '"') st.Push('"');
                        }
                        else
                        {
                            if (sd[ind] == '"') st.Pop();
                        }
                    }
                    else
                    {
                        if (sd[ind] == '(') st.Push('(');
                        if (sd[ind] == '"') st.Push('"');
                    }
                    tStr.Append(sd[ind]);
                    ind++;
                }
                if (!tStr.ToString().Equals(""))
                {
                    lst.Add(IsolateValuepair(tStr.ToString()));
                }
                sd.Remove(0, tStr.Length);
                if (sd.Length > 0 && sd[0] == ',')
                {
                    sd.Remove(0, 1);
                }
                ind = 0;
                tStr.Remove(0, tStr.Length);
            }
            return lst;
        }

        protected static List<String> RemoveEmpty(List<String> lst)
        {
            List<String> result = new List<string>();
            foreach (var st in lst)
            {
                if (!st.Equals("")) result.Add(st);
            }
            return result;
        }
        public static List<KVP> GetAgentData(String filename)
        {
            List<String> tsList;

            if (File.Exists(filename))
            {
                tsList = File.ReadAllLines(filename).ToList<String>();
                int i = 0;
                while (!tsList[i].StartsWith("visualize") && i < tsList.Count)
                {
                    i++;
                }
                String t = tsList[i].Replace(" ", "");
                StringBuilder ss = new StringBuilder(t);
                ss.Remove(0, 10);               // vsiualize([
                int tail = ss.Length - ss.ToString().IndexOf("],[") - 1;
                ss.Remove(ss.ToString().IndexOf("],[") + 1, tail);
                //ss.Remove(ss.ToString().IndexOf("],["), ss.Length -1- (ss.ToString().IndexOf("].[")));

                List<KVP> mekeke = IsolateList(ss.ToString());
                return mekeke;

            }

            return null;
        }

        public static List<KVP> GetItemData(String filename)
        {
            List<String> tsList;

            if (File.Exists(filename))
            {
                tsList = File.ReadAllLines(filename).ToList<String>();
                int i = 0;
                while (!tsList[i].StartsWith("visualize") && i < tsList.Count)
                {
                    i++;
                }
                String t = tsList[i].Replace(" ", "");
                StringBuilder ss = new StringBuilder(t);
                ss.Remove(0, "visualize([".Length);               // vsiualize([
                int head = ss.ToString().IndexOf("],[") + 2;
                ss.Remove(0, head);
                ss.Remove(ss.Length - 3, 2);
                //ss.Remove(ss.ToString().IndexOf("],["), ss.Length -1- (ss.ToString().IndexOf("].[")));

                List<KVP> mekeke = IsolateList(ss.ToString());
                return mekeke;

            }

            return null;
        }
        internal static bool GetSpecialData(string filename, out List<string> speciaList, out String specialName)
        {
            List<String> tsList;
            List<String> resuList = new List<string>();
            String grpName = "";
            if (File.Exists(filename))
            {
                tsList = File.ReadAllLines(filename).ToList<String>();
                int i = 0;
                while (!tsList[i].StartsWith("special_items") && i < tsList.Count)
                {
                    i++;
                }

                String line = tsList[i].Replace(" ", "");
                line = line.Replace("\r", "");
                line = line.Replace("\n", "");
                StringBuilder ss = new StringBuilder(line); //proverit', ne odli li tam chasy
                ss.Remove(0, "special_items(".Length); // 
                ss.Remove(ss.Length - 2, 2); //length of ");"

                StringBuilder sd = new StringBuilder(ss.ToString());
                Stack<Char> st = new Stack<Char>();
                StringBuilder tStr = new StringBuilder();
                int ind = 0;

                List<KVP> lst = new List<KVP>();

                while ((st.Count() != 0 && ind < sd.Length) || (ind == 0))
                {
                    if (sd[ind] == '(') st.Push('(');
                    if (sd[ind] == ')') st.Pop();
                    tStr.Append(sd[ind]);
                    ind++;
                }
                if (!tStr.ToString().Equals(""))
                {
                    lst.Add(IsolateValuepair(tStr.ToString()));
                }
                sd.Remove(0, tStr.Length);
                if (sd.Length > 0 && sd[0] == ',')
                {
                    sd.Remove(0, 1);
                }
                ind = 0;
                tStr.Remove(0, tStr.Length);
                // lst contains group name
                if (lst.Count > 0)
                {
                    String tst = lst[0].Value;
                    grpName = tst.Replace("\"", "");
                }


                sd.Remove(0, "(members".Length);
                String hr = sd.ToString(1, sd.ToString().Length - 2);
                if (hr.IndexOf("[") < 0)
                {
                    resuList.Add(hr.Replace("\"", ""));
                    hr = "";
                }
                else
                {
                    hr = hr.Replace("[", "");
                    hr = hr.Replace("]", "");
                    while (hr.IndexOf("\"") != hr.LastIndexOf("\""))
                    {
                        {

                            hr = hr.Substring(1, hr.Length - 1);
                            String tpt = hr.Substring(0, hr.IndexOf("\""));
                            resuList.Add(tpt.Replace("\"", ""));
                            hr = hr.Remove(0, hr.IndexOf("\"") + 1);
                            if (hr.Length > 0 && hr[0] == ',') hr = hr.Remove(0, 1);
                        }
                    }
                }
                speciaList = resuList;
                specialName = grpName;
                return true;


            }
            speciaList = null;
            specialName = "Error";
            return false;
        }

        public static bool DecipherMsgLine(String msg, out String outstr, out String tail)
        {
            String line = msg.Replace(" ", "");
            line = line.Replace("\r", "");
            line = line.Replace("\n", "");
            StringBuilder ss = new StringBuilder(line); //proverit', ne odli li tam chasy
            ss.Remove(0, 3); // length of "msg"
            int index = 0;
            Stack<Char> stack = new Stack<char>();
            StringBuilder newString = new StringBuilder();
            while (index < line.Length)  //msg example: msg(view(god, state([(clock, [(expired_at, next_tate_instantiation), (instance_of, event)])])), 21).
            {
                newString.Append(line[index]);
                if (line[index].Equals('(')) stack.Push('(');
                if (line[index].Equals(')'))
                {
                    if (stack.Count == 1)
                    {
                        outstr = newString.ToString() + '.';
                        tail = line.Length > outstr.Length ? line.Substring(outstr.Length) : "";
                        return true;
                    }
                    if (stack.Count > 1)
                    {
                        stack.Pop();
                    }
                    else
                    {
                        outstr = "";
                        tail = "";
                        return false;
                    }
                }
                index++;
            }
            outstr = "";
            tail = "";
            return false;

        }
    }
}
