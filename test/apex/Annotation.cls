// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_annotation.htm

global class MyClass {
     @future
     Public static void myMethod(String a)
     {
          //long-running Apex code
     }
}

public class AccountInsertAction {
  public static List<ID> insertAccounts(List<Account> accounts) {}
}