package org.bosik.diacomp.core.test.fakes.mocks;

import java.util.List;

public interface Mock<T>
{
	T getSample();

	List<T> getSamples();

	void compare(T exp, T act);
}
