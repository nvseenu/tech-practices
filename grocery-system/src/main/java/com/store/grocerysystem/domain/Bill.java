package com.store.grocerysystem.domain;

import java.text.SimpleDateFormat;
import java.util.Calendar;

public class Bill {

	private Calendar date;
	private Customer customer;
	private ItemSummary itemSummary;
	private DiscountSummary discountSummary;
	private double finalAmount;
	private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	private Bill() {
	}

	private void setDate(Calendar date) {
		this.date = date;
	}

	private void setCustomer(Customer customer) {
		this.customer = customer;
	}

	private void setItemSummary(ItemSummary itemSummary) {
		this.itemSummary = itemSummary;
	}

	private void setDiscountSummary(DiscountSummary discountSummary) {
		this.discountSummary = discountSummary;
	}

	private void setFinalAmount(double finalAmount) {
		this.finalAmount = finalAmount;
	}

	public Calendar getDate() {
		return date;
	}

	public Customer getCustomer() {
		return customer;
	}

	public ItemSummary getItemSummary() {
		return itemSummary;
	}

	public DiscountSummary getDiscountSummary() {
		return discountSummary;
	}

	public double getFinalAmount() {
		return finalAmount;
	}

	@Override
	public String toString() {
		return "Bill:\n" + dateFormat.format(date.getTime()) + "\n" + customer + "\n" + itemSummary + "\n"
				+ discountSummary + "\n finalAmount=" + finalAmount + "]";
	}

	public static class Builder {

		private Calendar date;
		private Customer customer;
		private double finalAmount;
		private DiscountSummary discountSummary;
		private ItemSummary itemSummary;

		public Builder() {

		}

		public Builder addCheckoutDate(Calendar date) {
			this.date = date;
			return this;
		}

		public Builder addCustomer(Customer customer) {
			this.customer = customer;
			return this;
		}

		public Builder addItemSummary(ItemSummary itemSummary) {
			this.itemSummary = itemSummary;
			return this;
		}

		public Builder addDiscountSummary(DiscountSummary discountSummary) {
			this.discountSummary = discountSummary;
			return this;
		}

		public Builder addFinalAmount(double amount) {
			finalAmount = amount;
			return this;
		}

		public Bill build() {
			Bill b = new Bill();
			b.setCustomer(customer);
			b.setDate(date);
			b.setItemSummary(itemSummary);
			b.setDiscountSummary(discountSummary);
			b.setFinalAmount(finalAmount);
			return b;
		}

	}

}
