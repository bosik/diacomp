package org.bosik.diacomp.android.backend.features.foodbase;

import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.services.foodbase.TestFoodbaseService;
import org.bosik.diacomp.core.services.foodbase.TestFoodbaseServiceCommon;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestFoodBaseLocalService extends AndroidTestCase implements TestFoodbaseService
{
	private TestFoodbaseServiceCommon	tester;

	@Override
	public void setUp()
	{
		tester = new TestFoodbaseServiceCommon()
		{
			@Override
			protected FoodBaseService getService()
			{
				return TestFoodBaseLocalService.this.getService();
			}
		};
		tester.init();
	}

	public FoodBaseService getService()
	{
		assertNotNull("Context is null", getContext());
		ContentResolver resolver = getContext().getContentResolver();
		return new FoodBaseLocalService(resolver);
	}

	@Override
	public void test_addFindById_single_PersistedOk()
	{
		tester.test_addFindById_single_PersistedOk();
	}

	@Override
	public void test_addFindAll_single_ReturnedNonEmpty()
	{
		tester.test_addFindChanged_single_ReturnedNonEmpty();
	}

	@Override
	public void test_addFindChanged_single_ReturnedNonEmpty()
	{
		tester.test_addFindChanged_single_ReturnedNonEmpty();
	}

	@Override
	public void test_delete_notFound_exceptionRaised()
	{
		tester.test_delete_notFound_exceptionRaised();
	}

	@Override
	public void test_addFindAny_single_ok()
	{
		tester.test_addFindAny_single_ok();
	}
}