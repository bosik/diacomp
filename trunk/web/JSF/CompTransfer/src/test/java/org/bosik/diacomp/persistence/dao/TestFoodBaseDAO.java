package org.bosik.diacomp.persistence.dao;

import junit.framework.TestCase;
import org.bosik.diacomp.bo.foodbase.FoodItem;
import org.bosik.diacomp.fakes.mocks.Mock;
import org.bosik.diacomp.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;

public abstract class TestFoodBaseDAO extends TestCase
{
	private FoodBaseDAO							foodBaseDAO;
	private static Mock<Versioned<FoodItem>>	mockGenerator	= new MockVersionedConverter<FoodItem>(
																		new MockFoodItem());

	protected abstract FoodBaseDAO getDAO();

	@Override
	protected void setUp()
	{
		foodBaseDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		// int version;

		if (foodBaseDAO.findById(org.getId()) != null)
		{
			// version = foodBaseDAO.getVersion();
			foodBaseDAO.delete(org.getId());
			// assertEquals(version + 1, foodBaseDAO.getVersion());
			assertNull(foodBaseDAO.findById(org.getId()));
		}

		// version = foodBaseDAO.getVersion();
		foodBaseDAO.add(org);
		// assertEquals(version + 1, foodBaseDAO.getVersion());

		// ------------------------------
		setUp();
		// ------------------------------

		Versioned<FoodItem> restored = foodBaseDAO.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		mockGenerator.compare(org, restored);

		// version = foodBaseDAO.getVersion();
		foodBaseDAO.delete(org.getId());
		// assertEquals(version + 1, foodBaseDAO.getVersion());
		assertNull(foodBaseDAO.findById(org.getId()));
	}

	// TODO: create testPersistenceMultiple()
}
