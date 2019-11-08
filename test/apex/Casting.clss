// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_casting.htm

public class CastingExample { 
      public void myfunc(){ 
            // Create a list of report objects
            Report[] Reports = new Report[5];

            // Create a custom report object
            CustomReport a = new CustomReport();

            Reports.add(a);

            CustomReport c = (CustomReport) Reports.get(0);
       }
}
