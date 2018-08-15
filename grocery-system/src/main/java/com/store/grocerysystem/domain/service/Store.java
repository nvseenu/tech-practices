package com.store.grocerysystem.domain.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.store.grocerysystem.domain.Bill;
import com.store.grocerysystem.domain.Cart;
import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.Items;
import com.store.grocerysystem.domain.Order;
import com.store.grocerysystem.domain.OrderSummary;
import com.store.grocerysystem.domain.PaymentType;

/**
 * 
 * This class represents a Grocery store. It provides features such as "select
 * items", "checkout" and "payment".
 * 
 * It takes care of inventory control like how many items are remaining etc. It
 * keeps each order placed.
 *
 */
public class Store {

	private Inventory inventory;
	private Register register;
	private List<Discount> discounts = new ArrayList<>();

	public Store(Inventory inventory, Register register) {
		this.inventory = inventory;
		this.register = register;
	}

	// Add items to the store. Generally it will be called before store ready for
	// checkout.
	public void addItems(Item item, int quantity) {
		inventory.addItems(item, quantity);
	}

	// Add discounts applicable. Generally it will be called before store ready for
	// checkout.
	public void addDiscounts(Collection<Discount> discounts) {
		this.discounts.addAll(discounts);
	}

	// Returns all available items in the store.
	public Collection<Items> getAllItems() {
		return inventory.getAllItems();
	}

	public Items getItems(String itemName) {
		return inventory.getItems(itemName);
	}

	// Returns a new cart to which items can be added.
	public Cart newCart() {
		return new Cart();
	}

	// Takes an item from a store.
	public Item takeItem(String name) {
		return inventory.takeItem(name);
	}

	// It will checkout given cart for given customer.
	// It will generate and return a bill after applying applicable discounts.
	public Bill checkout(Cart cart, Customer customer) {
		return register.checkout(cart, discounts, customer);
	}

	// Pays given bill and returns an order summary. We can think that order summary
	// is a printed bill.
	public Order receivePayment(Bill bill, PaymentType paymentType) {
		return register.receivePayment(bill, paymentType);
	}

	public Collection<Discount> getDiscounts() {
		return discounts;
	}

	public OrderSummary getOrderSummary() {
		return register.getOrderSummary();
	}

}
