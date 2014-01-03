package org.bosik.diacomp.resources;

import java.util.HashSet;
import java.util.Set;
import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

@ApplicationPath("/")
public class CompServerApp extends Application
{
	@Override
	public Set<Class<?>> getClasses()
	{
		final Set<Class<?>> classes = new HashSet<Class<?>>();
		// register root resource
		classes.add(AuthResource.class);
		classes.add(DiaryResource.class);
		return classes;
	}
}
