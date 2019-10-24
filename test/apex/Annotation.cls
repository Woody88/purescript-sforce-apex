// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_annotation.htm

global class MyClass {
     @future
     Public static void myMethod(String a)
     {
          //long-running Apex code
     }
}

public class AccountInsertAction {
  @InvocableMethod(label='Insert Accounts' description='Inserts the accounts specified and returns the IDs of the new accounts.')
  public static List<ID> insertAccounts(List<Account> accounts) {
    Database.SaveResult[] results = Database.insert(accounts);
    List<ID> accountIds = new List<ID>();
    for (Database.SaveResult result : results) {
      if (result.isSuccess()) {
        accountIds.add(result.getId());
      }
    }
    return accountIds;
  }
}