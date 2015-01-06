package org.bosik.diacomp.core.services.base.dish;

public interface TestDishbaseService
{
	void test_addFindById_single_PersistedOk();

	void test_addFindAll_single_ReturnedNonEmpty();

	void test_addFindChanged_single_ReturnedNonEmpty();

	void test_delete_notFound_exceptionRaised();

	void test_addFindAny_single_ok();
}
