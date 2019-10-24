// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_interfaces.htm

// An interface that defines what a purchase order looks like in general
public interface PurchaseOrder {
    // All other functionality excluded
    Double discount();
}

// One implementation of the interface for customers
public class CustomerPurchaseOrder implements PurchaseOrder {
    public Double discount() {
        return 0.05;  // Flat 5% discount
    }
}