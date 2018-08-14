package com.store.grocerysystem.domain;

public class OrderSummary {
	
	private Bill bill;
	private PaymentType paymentType;
	
	public OrderSummary(Bill bill, PaymentType paymentType) {
		this.bill = bill;
		this.paymentType = paymentType;
	}

	public Bill getBill() {
		return bill;
	}

	public PaymentType getPaymentType() {
		return paymentType;
	}	
}
