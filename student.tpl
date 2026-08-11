{% extends 'lab/index.html.j2' %}

{% block any_cell %}
{% if 'solution' not in cell.metadata.get('tags', []) %}
{{ super() }}
{% endif %}
{% endblock any_cell %}