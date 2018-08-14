package com.store.grocerysystem.domain;

import java.util.Calendar;
import java.util.List;

public class Register {

	private Integer id;
	private DiscountStrategy discountStrategy;

	public Register(int id, DiscountStrategy discountStrategy) {
		this.id = id;
		this.discountStrategy = discountStrategy;
	}

	public Integer getId() {
		return id;
	}

	public Bill checkout(Cart cart, List<Discount> discounts, Customer customer) {

		ItemSummary itemSummary = new ItemSummary(cart.getAllItems());

		// Apply discounts if any
		ItemSummary discountSummary = discountStrategy.apply(cart.getAllItems(), discounts, customer);
		double finalAmount = itemSummary.getAmount() - discountSummary.getAmount();

		Bill.Builder bb = new Bill.Builder();
		bb.addCheckoutDate(Calendar.getInstance()).addCustomer(customer).addItemSummary(itemSummary)
				.addDiscountSummary(discountSummary).addFinalAmount(finalAmount);
		return bb.build();
	}
}
