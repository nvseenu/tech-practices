package com.store.grocerysystem.domain.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.store.grocerysystem.domain.Bill;
import com.store.grocerysystem.domain.Cart;
import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.ItemEntries;
import com.store.grocerysystem.domain.ItemEntry;
import com.store.grocerysystem.domain.OrderSummary;
import com.store.grocerysystem.domain.PaymentType;

public class Store {

	private int itemSequence = 1;
	private Map<String, ItemEntries> itemsEntries = new HashMap<>();
	private Register register;
	private List<Discount> discounts = new ArrayList<>();
	
	public Store(Register register) {
		this.register = register;
	}

	public void addItems(Collection<Item> items) {
		items.forEach(item -> {
			addItem(item);
		});
	}

	public void addItem(Item item) {
		ItemEntry ie = new ItemEntry(itemSequence, item);
		ItemEntries entries = itemsEntries.get(ie.getItem().getName());
		if (entries == null) {
			entries = new ItemEntries();
			itemsEntries.put(ie.getItem().getName(), entries);
		}
		entries.add(ie);

	}

	public Collection<String> getAllItemNames() {
		return itemsEntries.keySet();
	}

	public Collection<Item> getItems(String itemName) {
		ItemEntries entries = itemsEntries.get(itemName);
		return entries.getItems();
	}

	public Cart newCart() {
		return new Cart();
	}

	public Item takeItem(String name) {
		ItemEntries entries = itemsEntries.get(name);
		return entries.removeOne().getItem();
	}

	public Bill checkout(Cart cart, Customer customer) {
		return register.checkout(cart, discounts, customer);
	}

	public OrderSummary payBill(Bill bill, PaymentType paymentType) {
		// Charge payment from given payment type
		OrderSummary summary = new OrderSummary(bill, paymentType);
		return summary;
	}

	public void addDiscounts(Collection<Discount> discounts) {
		this.discounts.addAll(discounts);
	}

	public Collection<Discount> getDiscounts() {
		return discounts;
	}
}
