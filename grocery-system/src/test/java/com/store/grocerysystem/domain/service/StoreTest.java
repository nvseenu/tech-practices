package com.store.grocerysystem.domain.service;

import static org.junit.Assert.assertEquals;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.store.grocerysystem.domain.Bill;
import com.store.grocerysystem.domain.Cart;
import com.store.grocerysystem.domain.Customer;
import com.store.grocerysystem.domain.Discount;
import com.store.grocerysystem.domain.DiscountType;
import com.store.grocerysystem.domain.Item;
import com.store.grocerysystem.domain.OrderSummary;
import com.store.grocerysystem.domain.PaymentType;
import com.store.grocerysystem.domain.service.DiscountStrategy;
import com.store.grocerysystem.domain.service.DiscountStrategyImpl;
import com.store.grocerysystem.domain.service.Register;
import com.store.grocerysystem.domain.service.Store;

public class StoreTest {

	@Test
	public void testCheckoutWithoutDiscounts() throws ParseException {
		Store store = createStore();
		store.addItems(prepareItems());

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
	public void testCheckoutWithItemDiscounts() throws ParseException {

		Store store = createStore();
		store.addItems(prepareItems());
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
		System.out.println(bill);
		assertEquals("Expected items are not added in the bill", items, bill.getItemSummary().getItems());
		assertEquals(finalAmount, bill.getFinalAmount(), 0.0);
	}

	@Test
	public void testPayment() throws ParseException {
		Store store = createStore();

		store.addItems(prepareItems());
		Cart cart = store.newCart();
		List<Item> items = new ArrayList<>();
		items.add(store.takeItem("Lays"));
		items.add(store.takeItem("TopRamen"));
		cart.addItems(items);

		Customer customer = new Customer(1, "Srinivasan", "10/01/1950", PaymentType.CASH);
		Bill bill = store.checkout(cart, customer);
		OrderSummary order = store.payBill(bill, customer.getPaymentType());
		assertEquals("Order does not contain a bill generated", bill, order.getBill());
		assertEquals("Order does not contain the payment type used", customer.getPaymentType(), order.getPaymentType());
	}

	private Store createStore() {
		DiscountStrategy discountStrategy = new DiscountStrategyImpl();
		Register register = new Register(1, discountStrategy);
		return new Store(register);
	}

	private List<Item> prepareItems() {
		List<Item> items = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			items.add(new Item("Maggi", 25.0, "Noodles"));
			items.add(new Item("TopRamen", 25.0, "Noodles"));
			items.add(new Item("Lays", 15.0, "Chips"));
			items.add(new Item("Bingo", 15.0, "Chips"));
		}
		return items;
	}
}
