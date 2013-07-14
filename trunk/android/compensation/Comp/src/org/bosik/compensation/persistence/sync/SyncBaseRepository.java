package org.bosik.compensation.persistence.sync;

import org.bosik.compensation.persistence.repository.common.BaseRepository;

public class SyncBaseRepository<T>
{
	public void synchronize(BaseRepository<T> source1, BaseRepository<T> source2)
	{
		int version1 = source1.getVersion();
		int version2 = source2.getVersion();

		if (version1 > version2)
			source2.postBase(source1.getBase());
		else
			if (version1 < version2)
				source1.postBase(source2.getBase());
	}
}
