 Please take a look at the class "com.store.grocerysystem.main.Main" which demonstrates all features of a grocery store.
 
 Please ensure you have below dependencies installed.
 Java 1.8
 Maven  
 
 You can run the class to see output of store basic operations.  
 
 
 Requirement analysis:
 Use cases:
 ----------
 As a store owner , i would like to add items, so that customer can order them.
 As a store owner, i would like to add registers , so that they can help customers to checkout their items. 
 
 As a customer, i would like to select the items from the store, so that i can buy them 
 As a store, i would like to checkout items customer has selected.   
 As a store, i would like to receive payment from customer for his/her items 
  
 As a store owner, i would like to set some 'x' discount rate to some specific items, so that customer will buy more and come back to 
 the store often. 
 As a store owner, i would like to set some 'x' discount rate to some item's category 
 As a store owner, i would like to give some 'x' discount to customers who are senior citizen. 
 As a store owner, i would like to give some 'x' discount to my employees on order to motivate them to continue working 
 with the store. 
  
 As a store owner , i would like to see how many items are available so that i can procure accordingly.
 As a store owner, i would like to see how many orders have been placed so that i can know about profit and loss of my store.

Users of the system:
-------------------
 - Customer
 - StoreOwner
 
 Store (system) 
 ---------------
 - initialize with items
 - Decides discounts
 - find items by name 
 - Give a cart for customers to add items
 - Checkout the items 
 - Find total sales
 - Find available items
 