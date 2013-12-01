package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.bo.foodbase.FoodItem;
import android.test.AndroidTestCase;

public abstract class FoodBaseDAOTest extends AndroidTestCase
{
	private BaseDAO<FoodItem>	foodBaseDAO;

	protected abstract BaseDAO<FoodItem> getDAO();

	@Override
	protected void setUp()
	{
		foodBaseDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		FoodItem org = new FoodItem();
		int version;

		org.setName("Хлеб \"Бородино\" нарезка (JUnit test)");
		org.setId("270C3EC853464B3DB314067AD005A727");
		org.setRelProts(5.5);
		org.setRelFats(0.9);
		org.setRelCarbs(44.1);
		org.setRelValue(206.3);
		org.setFromTable(false);

		if (foodBaseDAO.findById(org.getId()) != null)
		{
			version = foodBaseDAO.getVersion();
			foodBaseDAO.delete(org.getId());
			// assertEquals(version + 1, foodBaseDAO.getVersion());
			assertNull(foodBaseDAO.findById(org.getId()));
		}

		version = foodBaseDAO.getVersion();
		foodBaseDAO.add(org);
		// assertEquals(version + 1, foodBaseDAO.getVersion());

		setUp();

		// ------------------------------

		FoodItem restored = foodBaseDAO.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		FoodItemUtils.compareItems(org, restored);

		// ------------------------------

		version = foodBaseDAO.getVersion();
		foodBaseDAO.delete(org.getId());
		// assertEquals(version + 1, foodBaseDAO.getVersion());
		assertNull(foodBaseDAO.findById(org.getId()));
	}
}
