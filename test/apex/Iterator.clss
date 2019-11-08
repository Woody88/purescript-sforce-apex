// https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_iterable.htm
global class batchClass implements Database.batchable<Account>{ 
   global Iterable<Account> start(Database.batchableContext info){ 
       return new example(); 
   }     
   global void execute(Database.batchableContext info, List<Account> scope){ 
       List<Account> accsToUpdate = new List<Account>(); 
       for(Account a : scope){ 
           a.Name = 'true'; 
           a.NumberOfEmployees = 69; 
           accsToUpdate.add(a); 
       } 
       update accsToUpdate; 
   }     
   global void finish(Database.batchableContext info){ } 
}