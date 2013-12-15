package org.bosik.compensation.fakes.mocks;

import junit.framework.ComparisonFailure;
import junit.framework.TestCase;
import org.bosik.compensation.bo.basic.Versioned;
import org.bosik.compensation.bo.foodbase.FoodItem;
import android.util.Log;

public class FoodItemUtils extends TestCase
{
	private static final String	TAG	= FoodItemUtils.class.getSimpleName();
	private static final double	EPS	= 0.00001;

	public static FoodItem demoFoodItemA()
	{
		FoodItem demo = new FoodItem();

		demo.setName("Хлеб \"Бородино\" нарезка (JUnit test)");
		demo.setId("270C3EC853464B3DB314067AD005A727");
		demo.setRelProts(5.5);
		demo.setRelFats(0.9);
		demo.setRelCarbs(44.1);
		demo.setRelValue(206.3);
		demo.setFromTable(false);

		return demo;
	}

	public static void compareItems(FoodItem exp, FoodItem act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		try
		{
			assertEquals(exp, act);
			assertEquals(exp.getName(), act.getName());
			assertEquals(exp.getTag(), act.getTag());
			assertEquals(exp.getId(), act.getId());
			assertEquals(exp.getRelProts(), act.getRelProts(), EPS);
			assertEquals(exp.getRelFats(), act.getRelFats(), EPS);
			assertEquals(exp.getRelCarbs(), act.getRelCarbs(), EPS);
			assertEquals(exp.getRelValue(), act.getRelValue(), EPS);
			assertEquals(exp.getFromTable(), act.getFromTable());
		}
		catch (ComparisonFailure e)
		{
			Log.e(TAG, "Comparison error:");
			Log.e(TAG, exp.toString());
			Log.e(TAG, act.toString());
			throw e;
		}
	}

	public static void compareItems(Versioned<FoodItem> exp, Versioned<FoodItem> act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		try
		{
			assertEquals(exp, act);
			assertEquals(exp.getId(), act.getId());
			assertEquals(exp.getVersion(), act.getVersion());
			assertEquals(exp.getTimeStamp(), act.getTimeStamp());
		}
		catch (ComparisonFailure e)
		{
			Log.e(TAG, "Comparison error:");
			Log.e(TAG, exp.toString());
			Log.e(TAG, act.toString());
			throw e;
		}

		compareItems(exp.getData(), act.getData());
	}
}
