package org.bosik.compensation.fakes.mocks;

import java.util.ArrayList;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.compensation.bo.Food;
import org.bosik.compensation.utills.TestUtils;

public class MockFood implements Mock<Food>
{
	private static Food food(String name, double relProts, double relFats, double relCarbs, double relValue)
	{
		Food f = new Food();
		f.setName(name);
		f.setRelProts(relProts);
		f.setRelFats(relFats);
		f.setRelCarbs(relCarbs);
		f.setRelValue(relValue);
		return f;
	}

	public List<Food> getSamples()
	{
		List<Food> samples = new ArrayList<Food>();

		samples.add(food("Хлеб \"Бородино\" нарезка (JUnit test)", 5.5, 0.9, 44.1, 206.3));
		samples.add(food("Абрикос", 0.9, 0.1, 9, 41));
		samples.add(food("Аджика \"Кавказская\"", 0, 0, 15, 60));
		samples.add(food("Активиа кефирная 3%", 2.9, 3, 4.5, 56));
		samples.add(food("Батон \"К чаю\" ХЗ-2 (Золотой колос)", 7.4, 5.4, 51.2, 283));
		samples.add(food("Мороженое эскимо пломбир ванильный в молочном шоколаде \"Олимпиада\"", 4, 23, 27, 340));
		samples.add(food("Яйцо", 12.7, 11.5, 0.7, 157));

		return samples;
	}

	public void compare(Food exp, Food act)
	{
		TestCase.assertNotNull(exp);
		TestCase.assertNotNull(act);

		TestCase.assertEquals(exp.getName(), act.getName());
		TestCase.assertEquals(exp.getRelProts(), act.getRelProts(), TestUtils.EPS);
		TestCase.assertEquals(exp.getRelFats(), act.getRelFats(), TestUtils.EPS);
		TestCase.assertEquals(exp.getRelCarbs(), act.getRelCarbs(), TestUtils.EPS);
		TestCase.assertEquals(exp.getRelValue(), act.getRelValue(), TestUtils.EPS);
	}
}
