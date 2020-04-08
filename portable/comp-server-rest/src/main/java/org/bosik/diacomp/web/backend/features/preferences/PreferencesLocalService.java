/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.preferences;

import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferencesServiceContract;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

import static java.util.stream.Collectors.toList;

@Service
public class PreferencesLocalService
{
	@Autowired
	private PreferenceEntityRepository repository;

	public static PreferenceEntry<String> convert(PreferenceEntity e)
	{
		if (e == null)
		{
			return null;
		}

		final PreferenceEntry<String> result = new PreferenceEntry<>();

		result.setId(PreferenceID.parse(e.getId().getKey()));
		result.setValue(e.getValue());
		result.setVersion(e.getVersion());

		return result;
	}

	public static List<PreferenceEntry<String>> convert(List<PreferenceEntity> entities)
	{
		return entities.stream().map(PreferencesLocalService::convert).collect(toList());
	}

	private static void copyData(PreferenceEntry<String> source, PreferenceEntity destination)
	{
		destination.setValue(source.getValue());
		destination.setVersion(source.getVersion());
	}

	public String getHash(int userId)
	{
		return PreferencesServiceContract.getHash(getAll(userId));
	}

	public List<PreferenceEntry<String>> getAll(int userId)
	{
		return convert(repository.findByIdUserId(userId));
	}

	public void update(int userId, List<PreferenceEntry<String>> entries)
	{
		for (PreferenceEntry<String> entry : entries)
		{
			setString(userId, entry);
		}
	}

	public PreferenceEntry<String> getString(int userId, final PreferenceID id)
	{
		return convert(repository.findById(new PreferenceEntityPK(userId, id.getCode())).orElse(null));
	}

	private void setString(int userId, PreferenceEntry<String> entry)
	{
		PreferenceEntityPK pk = new PreferenceEntityPK(userId, entry.getId().getCode());
		PreferenceEntity entity = repository.findById(pk).orElse(null);

		if (entity == null)
		{
			entity = new PreferenceEntity();
			entity.setId(pk);
		}

		copyData(entry, entity);
		repository.save(entity);
	}

	public String exportJson(int userId)
	{
		List<PreferenceEntity> entities = repository.findByIdUserId(userId);

		StringBuilder s = new StringBuilder();
		s.append("[");

		for (PreferenceEntity entity : entities)
		{
			if (s.length() > 1)
			{
				s.append(",");
			}

			s.append(new JSONObject()
			{
				{
					put("key", entity.getId().getKey());
					put("value", entity.getValue());
					put("version", entity.getVersion());
				}
			}.toString());
		}

		s.append("]");
		return s.toString();
	}

	public String exportPlain(int userId)
	{
		List<PreferenceEntity> entities = repository.findByIdUserId(userId);

		StringBuilder s = new StringBuilder();
		s.append("VERSION=1\n");

		for (PreferenceEntity entity : entities)
		{
			s.append(entity.getId().getKey()).append('\t');
			s.append(entity.getVersion()).append('\t');
			s.append(entity.getValue()).append('\n');
		}

		return s.toString();
	}
}
