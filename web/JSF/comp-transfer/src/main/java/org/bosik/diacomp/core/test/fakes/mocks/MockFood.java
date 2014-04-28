package org.bosik.diacomp.core.test.fakes.mocks;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.bosik.diacomp.core.entities.business.Food;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.utils.Utils;

public class MockFood implements Mock<Food>
{
	private final Random	r	= new Random();

	@Override
	public List<Food> getSamples()
	{
		List<Food> samples = new ArrayList<Food>();

		samples.add(new Food("Хлеб \"Бородино\" нарезка (JUnit test)", 5.5, 0.9, 44.1, 206.3));
		samples.add(new Food("Абрикос", 0.9, 0.1, 9, 41));
		samples.add(new Food("Аджика \"Кавказская\"", 0, 0, 15, 60));
		samples.add(new Food("Активиа кефирная 3%", 2.9, 3, 4.5, 56));
		samples.add(new Food("Батон \"К чаю\" ХЗ-2 (Золотой колос)", 7.4, 5.4, 51.2, 283));
		samples.add(new Food("Мороженое эскимо пломбир ванильный в молочном шоколаде \"Олимпиада\"", 4, 23, 27, 340));
		samples.add(new Food("Яйцо", 12.7, 11.5, 0.7, 157));

		return samples;
	}

	@Override
	public Food getSample()
	{
		Food food = new FoodMassed();
		food.setName(Utils.randomString("Escape-test: %$\"'}{][#@!&`~/*-,.;", "Milk", "Абрикос",
				"Аджика \"Кавказская\"", "Активиа кефирная 3%", "Батон \"К чаю\" ХЗ-2 (Золотой колос)", "Вода",
				"Карбонат \"Восточный\" (Черн)",
				"Мороженое эскимо пломбир ванильный в молочном шоколаде \"Олимпиада\"", "Сахар",
				"Сметана \"Вкуснотеево\" 20%", "Хлеб \"Бородино\" нарезка", "Яйцо"));

		double relProts = r.nextInt(1000) / 10;
		double relFats = r.nextInt(1000) / 10;
		double relCarbs = r.nextInt(1000) / 10;

		double relSumm = relProts + relFats + relCarbs;
		relProts = Utils.round2(relProts / relSumm * 100);
		relFats = Utils.round2(relFats / relSumm * 100);
		relCarbs = Utils.round2(relCarbs / relSumm * 100);

		double relValue = Utils.round2((relProts * Utils.KCAL_PER_PROTS) + (relFats * Utils.KCAL_PER_FATS)
				+ (relCarbs * Utils.KCAL_PER_CARBS));

		food.setRelProts(relProts);
		food.setRelFats(relFats);
		food.setRelCarbs(relCarbs);
		food.setRelValue(relValue);

		return food;
	}

	@Override
	public void compare(Food exp, Food act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		assertEquals(exp.getName(), act.getName());
		assertEquals(exp.getRelProts(), act.getRelProts(), Utils.EPS);
		assertEquals(exp.getRelFats(), act.getRelFats(), Utils.EPS);
		assertEquals(exp.getRelCarbs(), act.getRelCarbs(), Utils.EPS);
		assertEquals(exp.getRelValue(), act.getRelValue(), Utils.EPS);
	}
}
