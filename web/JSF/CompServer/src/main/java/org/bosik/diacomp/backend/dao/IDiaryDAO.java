package org.bosik.diacomp.backend.dao;

import java.util.Date;
import java.util.List;

import org.bosik.diacomp.persistence.common.Versioned;

public interface IDiaryDAO
{
	List<Versioned<String>> findMod(int userId, Date time);
}
