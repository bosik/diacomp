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

import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.features.user.auth.UserRest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.core.MediaType;
import java.util.List;

@RestController
@RequestMapping("/preferences")
public class PreferencesRest extends UserRest
{
	private static final String TYPE_JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=utf-8";

	private final Serializer<PreferenceEntry<String>> serializer = new SerializerAdapter<>(new ParserPreferenceEntry());

	@Autowired
	private PreferencesLocalService preferencesService;

	@GetMapping(produces = TYPE_JSON_UTF8)
	public List<PreferenceEntry<String>> getAll()
	{
		return preferencesService.getAll(getUserId());
	}

	@GetMapping(path = "/{key}", produces = TYPE_JSON_UTF8)
	public PreferenceEntry<String> getPreference(@PathVariable(name = "key") String parKey)
	{
		return preferencesService.getString(getUserId(), PreferenceID.parse(parKey));
	}

	@GetMapping(path = "/hash")
	public String getHash()
	{
		return Utils.nullToEmpty(preferencesService.getHash(getUserId()));
	}

	@PutMapping
	public String update(@RequestParam(name = "data") String parData)
	{
		List<PreferenceEntry<String>> entries = serializer.readAll(parData);
		preferencesService.update(getUserId(), entries);
		return "Saved OK";
	}
}
