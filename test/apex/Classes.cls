// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_understanding.htm 
public class Plotter {
    public Integer myfunc(Double x){
        this.x = x;
    }
    // This inner class manages the points
    class Point {
        Double x;
        Double y;

        Point(Double x, Double y) {
             this.x = x;
             this.y = y;
        }
        Double getXCoordinate() {
             return x;
        }

        Double getYCoordinate() {
             return y;
        }
    }

    List<Point> points = new List<Point>();

    public void plot(Double x, Double y) {
        points.add(new Point(x, y));
    }
    
    // The following method takes the list of points and does something with them
    public void render() {
    }
}

// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_extending.htm
public virtual class Marker {
    public virtual void write() {
        System.debug('Writing some text.');
    }

    public virtual Double discount() {
        return 0.05;
    }
}

// Extension for the Marker class
public class YellowMarker extends Marker {
    public override void write() {
        System.debug('Writing some text using the yellow marker.');
    } 
}    

// Extension for the Marker class
public class RedMarker extends Marker {
    public override void write() {
        System.debug('Writing some text in red.');
    } 

    // Method only in this class
    public Double computePrice() {
        return 1.5;
    }
}    