package org.bosik.diacomp.core.entities.business;

import junit.framework.TestCase;
import org.bosik.diacomp.core.utils.Utils;

public class TestFood extends TestCase
{
	private Food	food	= new Food();

	public void testName()
	{
		// нормальный тест
		food.setName("name");
		assertEquals("name", food.getName());

		// краш-тест
		//		try
		//		{
		//			food.setName(null);
		//			fail();
		//		}
		//		catch (IllegalArgumentException e)
		//		{
		//		}
		//		try
		//		{
		//			food.setName("  ");
		//			fail();
		//		}
		//		catch (IllegalArgumentException e)
		//		{
		//		}
		//		try
		//		{
		//			food.setName("");
		//			fail();
		//		}
		//		catch (IllegalArgumentException e)
		//		{
		//		}
	}

	public void testRelProts()
	{
		// нормальный тест
		food.setRelProts(0.0);
		assertEquals(0.0, food.getRelProts(), Utils.EPS);
		food.setRelProts(12.8);
		assertEquals(12.8, food.getRelProts(), Utils.EPS);
		food.setRelProts(100.0);
		assertEquals(100.0, food.getRelProts(), Utils.EPS);

		// краш-тест
		try
		{
			food.setRelProts(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelProts(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelFats()
	{
		// нормальный тест
		food.setRelFats(0.0);
		assertEquals(0.0, food.getRelFats(), Utils.EPS);
		food.setRelFats(12.8);
		assertEquals(12.8, food.getRelFats(), Utils.EPS);
		food.setRelFats(100.0);
		assertEquals(100.0, food.getRelFats(), Utils.EPS);

		// краш-тест
		try
		{
			food.setRelFats(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelFats(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelCarbs()
	{
		// нормальный тест
		food.setRelCarbs(0.0);
		assertEquals(0.0, food.getRelCarbs(), Utils.EPS);
		food.setRelCarbs(12.8);
		assertEquals(12.8, food.getRelCarbs(), Utils.EPS);
		food.setRelCarbs(100.0);
		assertEquals(100.0, food.getRelCarbs(), Utils.EPS);

		// краш-тест
		try
		{
			food.setRelCarbs(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setRelCarbs(100.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testRelValue()
	{
		// нормальный тест
		food.setRelValue(0.0);
		assertEquals(0.0, food.getRelValue(), Utils.EPS);
		food.setRelValue(12.8);
		assertEquals(12.8, food.getRelValue(), Utils.EPS);
		food.setRelValue(102.8);
		assertEquals(102.8, food.getRelValue(), Utils.EPS);

		// краш-тест
		try
		{
			food.setRelValue(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}
}