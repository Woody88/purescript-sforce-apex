/* https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_declaring_variables.htm 
   https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_constructors.htm
*/
public class MyClass { 
    public final boolean b = true;
    integer t = 12;
    double dfa { 
        public get { return 10.00; }  
        private set { dfa = 1.12; }
    }
    public long l = 1231123L;
    public string myvar {
        get;
        set { myvar = 'Hello, World!'; }
    }
    public static id myobjId = '00300000003T2PGAA0';

}

public static class MyClass2 {
    class MemberClass { }

    class MemberClass2 { 
        double k = 12.21;
    }
}


public class MyClass3 {
    public interface PurchaseOrder {}

    public interface PurchaseOrder2 {
        Double discount();
    }
}

public class Leads {

  // First a no-argument constructor 
  public Leads () {}

  // A constructor with one argument
  public Leads (Integer call) {}

  // A constructor with two arguments
  public Leads (String email, Boolean call) {}

  // Though this constructor has the same arguments as the 
  // one above, they are in a different order, so this is legal
  public Leads (Boolean call, String email) {}
}

public class TestObject2 {

    //private static final Integer DEFAULT_SIZE = 10;
    Integer size;

    //Constructor with no arguments
    public TestObject2() {
        this(DEFAULT_SIZE); // Using this(...) calls the one argument constructor    
    }

    // Constructor with one argument 
    public TestObject2 (Integer ObjectSize) {
        size = ObjectSize;  
    }
}
