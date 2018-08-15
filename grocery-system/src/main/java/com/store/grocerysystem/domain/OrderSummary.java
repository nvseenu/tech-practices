package com.store.grocerysystem.domain;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * It keeps all orders placed.
 *
 */
public class OrderSummary {

	private List<Order> orders = new ArrayList<>();
	private int totalOrders;
	private double totalAmount;

	public OrderSummary(Collection<Order> orders) {
		this.orders.addAll(orders);
		totalOrders = orders.size();
		for (Order o : orders) {
			totalAmount += o.getBill().getFinalAmount();
		}
	}

	public Collection<Order> getOrders() {
		return Collections.unmodifiableCollection(orders);
	}

	public int getTotalOrders() {
		return totalOrders;
	}

	public double getTotalAmount() {
		return totalAmount;
	}

	@Override
	public String toString() {
		return "OrderSummary [orders=" + orders + ", totalOrders=" + totalOrders + ", totalAmount=" + totalAmount + "]";
	}
}
