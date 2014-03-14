package org.bosik.diacomp.core.services.foodbase;

import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.utils.test.fakes.mocks.Mock;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockFoodItem;
import org.bosik.diacomp.core.utils.test.fakes.mocks.MockVersionedConverter;

public abstract class TestFoodbaseServiceCommon extends TestCase implements TestFoodbaseService
{
	private FoodBaseService						foodBaseService;
	private static Mock<Versioned<FoodItem>>	mockGenerator	= new MockVersionedConverter<FoodItem>(
																		new MockFoodItem());

	protected abstract FoodBaseService getService();

	@Override
	protected void setUp()
	{
		foodBaseService = getService();
	}

	@Override
	public void test_addFindById_single_PersistedOk()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);

		if (foodBaseService.findById(org.getId()) == null)
		{
			foodBaseService.add(org);
		}

		// ------------------------------
		setUp();
		// ------------------------------

		Versioned<FoodItem> restored = foodBaseService.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		mockGenerator.compare(org, restored);
	}

	// TODO: create testPersistenceMultiple()
}
