using System;
using System.Collections.Generic;
using System.Data.SqlTypes;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Data;

namespace CrystalFire
{
    class LineNumberToSliderConvertor :IValueConverter
    {

        public static int FieldCount = 0;
        public static int FieldDuration = 0;
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            int  tp = (int?) value != null ? (int) value : 0;

            
            int num = Int32.Parse(parameter.ToString());

            
            int t10 = (int) (Math.Floor((double) tp/10)*10) +10;

            int tn = t10/10*num;


            return tn;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return null;
        }
    }
}
