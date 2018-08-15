package com.store.grocerysystem.domain;

/**
 * This class represents an order which contains a bill and payment details.
 *
 */
public class Order {
	
	private Bill bill;
	private PaymentType paymentType;
	
	public Order(Bill bill, PaymentType paymentType) {
		this.bill = bill;
		this.paymentType = paymentType;
	}

	public Bill getBill() {
		return bill;
	}

	public PaymentType getPaymentType() {
		return paymentType;
	}

	@Override
	public String toString() {
		return "Order:\n" + bill + "\n" + paymentType;
	}	
	
	
}
