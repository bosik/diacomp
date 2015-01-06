package org.bosik.diacomp.android.backend.features.dishbase;

import org.bosik.diacomp.core.services.base.dish.DishBaseService;
import org.bosik.diacomp.core.services.base.dish.TestDishbaseService;
import org.bosik.diacomp.core.services.base.dish.TestDishbaseServiceCommon;
import android.content.ContentResolver;
import android.test.AndroidTestCase;

public class TestDishBaseLocalService extends AndroidTestCase implements TestDishbaseService
{
	private TestDishbaseServiceCommon	tester;

	@Override
	public void setUp() throws Exception
	{
		super.setUp();
		tester = new TestDishbaseServiceCommon()
		{
			@Override
			protected DishBaseService getService()
			{
				return TestDishBaseLocalService.this.getService();
			}
		};
		tester.init();
	}

	public DishBaseService getService()
	{
		assertNotNull("Context is null", getContext());
		ContentResolver resolver = getContext().getContentResolver();
		return new DishBaseLocalService(resolver);
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