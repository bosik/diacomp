package org.bosik.diacomp.bo.common;

import junit.framework.TestCase;
import org.bosik.diacomp.bo.FoodMassed;
import org.bosik.diacomp.utills.TestUtils;

public class TestFoodMassed extends TestCase
{
	private FoodMassed	food	= new FoodMassed();

	// TODO: change Exception to IllegalArgumentException

	public void testMass()
	{
		// normal test
		food.setMass(0.0);
		assertEquals(0.0, food.getMass(), TestUtils.EPS);
		food.setMass(0.01);
		assertEquals(0.01, food.getMass(), TestUtils.EPS);
		food.setMass(100.0);
		assertEquals(100.0, food.getMass(), TestUtils.EPS);
		food.setMass(999.5);
		assertEquals(999.5, food.getMass(), TestUtils.EPS);

		// crash-test
		try
		{
			food.setMass(-0.01);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
		try
		{
			food.setMass(-100);
			fail();
		}
		catch (IllegalArgumentException e)
		{
		}
	}

	public void testGetAbs()
	{
		// normal test
		food.setMass(10);

		food.setRelProts(1);
		assertEquals(0.1, food.getProts(), TestUtils.EPS);
		food.setRelFats(2);
		assertEquals(0.2, food.getFats(), TestUtils.EPS);
		food.setRelCarbs(3);
		assertEquals(0.3, food.getCarbs(), TestUtils.EPS);
		food.setRelValue(4);
		assertEquals(0.4, food.getValue(), TestUtils.EPS);

		food.setMass(1000);

		food.setRelProts(2);
		assertEquals(20.0, food.getProts(), TestUtils.EPS);
		food.setRelFats(4);
		assertEquals(40.0, food.getFats(), TestUtils.EPS);
		food.setRelCarbs(6);
		assertEquals(60.0, food.getCarbs(), TestUtils.EPS);
		food.setRelValue(8);
		assertEquals(80.0, food.getValue(), TestUtils.EPS);

		food.setMass(0);

		assertEquals(0.0, food.getProts(), TestUtils.EPS);
		assertEquals(0.0, food.getFats(), TestUtils.EPS);
		assertEquals(0.0, food.getCarbs(), TestUtils.EPS);
		assertEquals(0.0, food.getValue(), TestUtils.EPS);

		// crash-test
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
