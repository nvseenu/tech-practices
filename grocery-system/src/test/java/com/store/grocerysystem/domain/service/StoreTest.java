package com.store.grocerysystem.domain.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.store.grocerysystem.domain.Bill;
import com.store.grocerysystem.domain.Cart;
import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.DiscountType;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.Order;
import com.store.grocerysystem.domain.OrderSummary;
import com.store.grocerysystem.domain.PaymentType;

public class StoreTest {

	private Store store;

	@Before
	public void setup() {
		Inventory inventory = new Inventory();
		Register register = new Register("Register001");
		store = new Store(inventory, register);
		store.addItems(new Item("Maggi", 25.0, "Noodles"), 5);
		store.addItems(new Item("TopRamen", 25.0, "Noodles"), 5);
		store.addItems(new Item("Lays", 15.0, "Chips"), 5);
		store.addItems(new Item("Bingo", 15.0, "Chips"), 5);
	}

	@Test
	public void testSetup() {
		List<Discount> discounts = new ArrayList<>();
		discounts.add(new Discount("Maggi", 5, DiscountType.PER_ITEM));
		discounts.add(new Discount(10, DiscountType.SENIOR_CITIZEN));

		store.addDiscounts(discounts);
		assertEquals("Total Lays items is not same as what we set", 5, store.getItems("Lays").getQuantity());
		assertEquals("Total TopRamen items is not same as what we set", 5, store.getItems("TopRamen").getQuantity());
		assertEquals("Total Maggi items is not same as what we set", 5, store.getItems("Maggi").getQuantity());
		assertEquals("Total Bingo items is not same as what we set", 5, store.getItems("Bingo").getQuantity());
		assertEquals("Discounts are not same as what we set", discounts, store.getDiscounts());
	}

	@Test
	public void testCheckoutWithoutDiscounts() {

		Cart cart = store.newCart();
		List<Item> items = new ArrayList<>();
		items.add(store.takeItem("Lays"));
		items.add(store.takeItem("TopRamen"));
		cart.addItems(items);

		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		Bill bill = store.checkout(cart, customer);
		assertEquals("Expected items are not added in the bill", items, bill.getItemSummary().getItems());
		assertEquals(40.0d, bill.getFinalAmount(), 0.0);
	}

	@Test
	public void testCheckoutWithItemDiscounts() {

		List<Discount> discounts = new ArrayList<>();
		discounts.add(new Discount("Maggi", 5, DiscountType.PER_ITEM));
		discounts.add(new Discount(10, DiscountType.SENIOR_CITIZEN));

		store.addDiscounts(discounts);

		Cart cart = store.newCart();
		List<Item> items = new ArrayList<>();
		items.add(store.takeItem("Lays"));
		items.add(store.takeItem("TopRamen"));
		items.add(store.takeItem("Maggi"));
		cart.addItems(items);

		double totalAmount = 65.0;
		double maggiDiscount = 25.0 * 5 * 0.01;
		double seniorCitizenDiscount = 65 * 10 * 0.01;
		double finalAmount = totalAmount - (maggiDiscount + seniorCitizenDiscount);

		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		Bill bill = store.checkout(cart, customer);
		assertEquals("Expected items are not added in the bill", items, bill.getItemSummary().getItems());
		assertEquals(finalAmount, bill.getFinalAmount(), 0.0);
	}

	@Test
	public void testPayment() {
		Cart cart = store.newCart();
		List<Item> items = new ArrayList<>();
		items.add(store.takeItem("Lays"));
		items.add(store.takeItem("TopRamen"));
		cart.addItems(items);

		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		Bill bill = store.checkout(cart, customer);
		Order order = store.receivePayment(bill, customer.getPaymentType());
		assertEquals("Order does not contain a bill generated", bill, order.getBill());
		assertEquals("Order does not contain the payment type used", customer.getPaymentType(), order.getPaymentType());
	}

	@Test
	public void testAvaialbleItemsAfterSomeCheckouts() {
		Cart cart = store.newCart();
		List<Item> items = new ArrayList<>();
		items.add(store.takeItem("Lays"));
		items.add(store.takeItem("Lays"));
		items.add(store.takeItem("TopRamen"));
		items.add(store.takeItem("TopRamen"));
		cart.addItems(items);

		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		store.checkout(cart, customer);

		assertEquals("Total Lays items is not reduced after checkout", 3, store.getItems("Lays").getQuantity());
		assertEquals("Total TopRamen items is not reduced after checkout", 3, store.getItems("TopRamen").getQuantity());
		assertEquals("Total Maggi items does not remain same after checkout, as it has not been selected", 5,
				store.getItems("Maggi").getQuantity());
		assertEquals("Total Bingo items does not remain same after checkout, as it has not been selected", 5,
				store.getItems("Bingo").getQuantity());

	}
	
	@Test
	public void testTotalOrdersPlaced() {
		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);

		Cart cart = store.newCart();
		cart.addItem(store.takeItem("Lays"));
		Bill bill = store.checkout(cart, customer);
		Order order1 = store.receivePayment(bill, PaymentType.CASH);
		
		cart = store.newCart();
		cart.addItem(store.takeItem("Lays"));
		bill = store.checkout(cart, customer);
		Order order2 = store.receivePayment(bill, PaymentType.CASH);
		
		OrderSummary orderSummary = store.getOrderSummary();
		assertEquals("Total orders placed are wrong", 2, orderSummary.getTotalOrders());
		assertTrue("First order is not found", orderSummary.getOrders().contains(order1));
		assertTrue("Second order is not found", orderSummary.getOrders().contains(order2));
		
	}

}
