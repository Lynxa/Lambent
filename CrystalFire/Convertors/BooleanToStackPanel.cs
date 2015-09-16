using System;
using System.Collections.Generic;
using System.Data.SqlTypes;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Media;
using System.Windows.Shapes;

namespace CrystalFire
{
    class BooleanToStackPanel : IValueConverter
    {

        public static int FieldCount = 0;
        public static int FieldDuration = 0;
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            ExecutionState  tp = (ExecutionState) value;

            StackPanel pausePanel = new StackPanel();

            if (tp == ExecutionState.Running)
            {
                pausePanel.Orientation = Orientation.Horizontal;
                Polygon pause1 = new Polygon();
                pause1.Points = new PointCollection(new List<Point>() { new Point(0, 0), new Point(3, 0), new Point(3, 14), new Point(0, 14) });
                pause1.Fill = Brushes.Black;
                pause1.Stroke = Brushes.Black;
                Polygon pause2 = new Polygon();
                pause2.Points = new PointCollection(new List<Point>() { new Point(5, 0), new Point(8, 0), new Point(8, 14), new Point(5, 14) });
                pause2.Fill = Brushes.Black;
                pause2.Stroke = Brushes.Black;

                pausePanel.Children.Add(pause1);
                pausePanel.Children.Add(pause2);
            }

            else if (tp == ExecutionState.Paused || tp == ExecutionState.Void)
            {
                Polygon runIcon = new Polygon();
                runIcon.Points = new PointCollection(new List<Point>() { new Point(0, 0), new Point(11, 7), new Point(0, 14) });
                runIcon.Fill = Brushes.Black;
                runIcon.Stroke = Brushes.Black;
                pausePanel.Children.Add(runIcon);
            }
            return pausePanel;

        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return null;
        }
    }
}
