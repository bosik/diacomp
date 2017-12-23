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
package org.bosik.diacomp.android.backend.features.preferences.account;

import java.util.ArrayList;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.diacomp.android.backend.common.webclient.WebClient;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.preferences.PreferenceID;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesService;

public class PreferencesWebService extends PreferencesService
{
	// REST methods
	private static final String							API_PREFERENCES			= "api/preferences/";
	private static final String							API_PREFERENCES_HASH	= "api/preferences/hash";
	private static final String							API_PREFERENCES_KEY		= "api/preferences/%s";

	private final WebClient								webClient;
	private final Parser<PreferenceEntry<String>>		parser					= new ParserPreferenceEntry();
	private final Serializer<PreferenceEntry<String>>	serializer				= new SerializerAdapter<PreferenceEntry<String>>(
																						parser);

	public PreferencesWebService(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new IllegalArgumentException("WebClient is null");
		}

		this.webClient = webClient;
	}

	@Override
	public String getHash()
	{
		try
		{
			String query = API_PREFERENCES_HASH;
			String resp = webClient.get(query);
			return resp;
		}
		catch (CommonServiceException e)
		{
			throw e;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<PreferenceEntry<String>> getAll()
	{
		try
		{
			String url = API_PREFERENCES;
			String resp = webClient.get(url);
			return serializer.readAll(resp);
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void update(List<PreferenceEntry<String>> entries)
	{
		if (entries.isEmpty())
		{
			return;
		}

		try
		{
			List<NameValuePair> params = new ArrayList<NameValuePair>();
			params.add(new BasicNameValuePair("data", serializer.writeAll(entries)));
			webClient.put(API_PREFERENCES, params);
		}
		catch (Exception e)
		{
			throw new CommonServiceException("URL: " + API_PREFERENCES, e);
		}
	}

	@Override
	public PreferenceEntry<String> getString(PreferenceID id)
	{
		try
		{
			String url = String.format(API_PREFERENCES_KEY, id.getKey());
			String resp = webClient.get(url);
			return serializer.read(resp);
		}
		catch (NotFoundException e)
		{
			return null;
		}
		catch (Exception e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void setString(PreferenceEntry<String> entry)
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
