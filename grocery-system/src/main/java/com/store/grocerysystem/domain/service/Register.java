package com.store.grocerysystem.domain.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import com.store.grocerysystem.domain.Bill;
import com.store.grocerysystem.domain.Cart;
import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.DiscountSummary;
import com.store.grocerysystem.domain.ItemSummary;
import com.store.grocerysystem.domain.Order;
import com.store.grocerysystem.domain.OrderSummary;
import com.store.grocerysystem.domain.PaymentType;

/**
 * This class represents a register machine in grocery store domain. It takes
 * care of generating bills, payments and stores orders placed.
 * 
 */
public class Register {

	private String id;
	private DiscountCalc discountCalc = new DiscountCalc();
	private List<Order> orders = new ArrayList<>();

	public Register(String id) {
		this.id = id;
	}

	public String getId() {
		return id;
	}

	/**
	 * Generates a bill for given cart for given customer. This process is called as
	 * checkout. A generated bill will contain items selected, discounts applied and
	 * final amount to pay.
	 * 
	 * @param cart
	 * @param discounts
	 * @param customer
	 * @return a bill
	 */
	public Bill checkout(Cart cart, List<Discount> discounts, Customer customer) {
		ItemSummary itemSummary = new ItemSummary(cart.getAllItems());

		// Apply discounts if any
		DiscountSummary discountSummary = discountCalc.apply(cart.getAllItems(), discounts, customer);
		double finalAmount = itemSummary.getAmount() - discountSummary.getAmount();

		Bill.Builder bb = new Bill.Builder();
		bb.addCheckoutDate(Calendar.getInstance()).addCustomer(customer).addItemSummary(itemSummary)
				.addDiscountSummary(discountSummary).addFinalAmount(finalAmount);
		return bb.build();
	}

	/**
	 * Receives payment from the customer for given bill. Generates an order which
	 * is equivalent to a printed bill.
	 * 
	 * @param bill
	 * @param paymentType
	 * @return An order summary
	 */
	public Order payBill(Bill bill, PaymentType paymentType) {
		Order order = new Order(bill, paymentType);
		orders.add(order);
		return order;
	}

	public OrderSummary getOrderSummary() {
		return new OrderSummary(this.orders);
	}
}
