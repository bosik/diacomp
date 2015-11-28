/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2013 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package org.bosik.diacomp.android.backend.features.diary;

import org.bosik.diacomp.core.services.diary.DiaryService;
import android.content.ContentResolver;
import android.util.Log;

public class LocalDiary
{
	private static final String	TAG	= LocalDiary.class.getSimpleName();
	private static DiaryService	instance;

	public static synchronized DiaryService getInstance(ContentResolver resolver)
	{
		if (null == instance)
		{
			Log.i(TAG, "Local diary initialization...");
			instance = new DiaryLocalService(resolver);
		}
		return instance;
	}
}